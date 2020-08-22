;; This is an experimental AWS mode I'm working on _UNOFFICIALLY_. Use
;; at your own risk :)

(require 'json)
(require 'seq)
(require 'org)

(add-to-list 'org-src-lang-modes '("sql-athena" . sql))

(defvar aws-console/athena/invalid-query-prefix
  "Invalid Query: ")

(defun aws-console/athena/result-response-to-table (response-json)
  (let* ((result-json (cdr (assoc 'ResultSet response-json)))
         (raw-rows (cdr (assoc 'Rows result-json)))
         (rows (mapcar (lambda (raw-row)
                         (cdr (assoc 'Data raw-row)))
                       raw-rows)))
    (mapcar (lambda (row)
              (mapcar (lambda (raw-value)
                        (cdr (assoc 'VarCharValue raw-value)))
                      row))
            rows)))

(defun aws-console/athena/wait-for-query-result (query-id)
  (let ((result nil)
        (response nil))
    (while (not result)
      (sleep-for 2)
      (setq response (aws-console/athena/get-query-result query-id))
      (setq result
            (when (or (not (stringp response))
                      (not (string-match-p "\\(QUEUED\\|RUNNING\\)" response)))
              response)))
    result))

(defun org-babel-execute:sql-athena (body params)
  "Execute a block of Athena (SQL) code with Babel.
  This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-python-initiate-session
                   (cdr (assoc :session params))))
         (query-id-response (aws-console/athena/execute-query body))
         (query-result
          (if (string-prefix-p aws-console/athena/invalid-query-prefix
                               query-id-response)
              query-id-response ;; Invalid query, dump the response
            (aws-console/athena/wait-for-query-result query-id-response)))
         (org-output (if (string-prefix-p
                          aws-console/athena/invalid-query-prefix
                          query-result)
                         query-result ;; Invalid query, dump string
                       (aws-console/athena/result-response-to-table
                        query-result))))
    (org-babel-reassemble-table
     org-output
     (org-babel-pick-name (cdr (assoc :colname-names params))
                          (cdr (assoc :colnames params)))
     (org-babel-pick-name (cdr (assoc :rowname-names params))
                          (cdr (assoc :rownames params))))))

(defun aws-console/athena/get-query-result (query-id)
  (let* ((query-response (shell-command-to-string
                          (concat "aws athena get-query-results "
                                  "--query-execution-id "
                                  (shell-quote-argument query-id))))
         (is-valid-query (aws-console/validate-json-string query-response)))
    (if is-valid-query
        (json-read-from-string query-response)
      (concat aws-console/athena/invalid-query-prefix query-response))))

(defun aws-console/athena/execute-query (query)
  (let* ((query-response (shell-command-to-string
                          (concat "aws athena start-query-execution "
                                  "--query-string "
                                  (shell-quote-argument query))))
         (is-valid-query (aws-console/validate-json-string query-response))
         (query-id (if is-valid-query
                       (cdr (assoc 'QueryExecutionId
                                   (json-read-from-string query-response)))
                     nil)))
    (if is-valid-query
        query-id
      (concat aws-console/athena/invalid-query-prefix query-response))))

(defun aws-console/validate-json-string (string)
  "Validates a JSON string."
  (condition-case nil
      (progn
        (json-read-from-string string)
        t)
    (error nil)))

(defun aws-console/glue/start-crawler (&optional name)
  (interactive)
  (message (shell-command-to-string
            (concat "aws glue start-crawler --name "
                    (shell-quote-argument
                     (if name name (read-string "Crawler name: ")))))))

(defun aws-console/glue/get-database-names ()
  (let* ((db-json-string (shell-command-to-string "aws glue get-databases"))
         (db-json (json-read-from-string db-json-string))
         (db-entries (cdr (assoc 'DatabaseList db-json))))
    (mapcar
     (lambda (entry) (cdr (assoc 'Name entry)))
     db-entries)))

(defun aws-console/glue/get-table-names (db)
  (let* ((table-json-string
          (shell-command-to-string
           (format "aws glue get-tables --database-name %s" db)))
         (table-json (json-read-from-string table-json-string))
         (table-entries (cdr (assoc 'TableList table-json))))
    (mapcar
     (lambda (entry) (cdr (assoc 'Name entry)))
     table-entries)))

(defun aws-console/glue/get-table (db table)
  (let* ((table-json-string
          (shell-command-to-string
           (format "aws glue get-table --database-name %s --name %s" db table)))
         (table-json (json-read-from-string table-json-string)))
    (cdr (assoc 'Table table-json))))

(defun aws-console/glue/delete-table (database table)
  (shell-command-to-string
   (format "aws glue delete-table --database-name %s --name %s" database table))
  (message (format "Table %s deleted from %s" table database)))

(defun aws-console/glue/show-table-schema (database table)
  (let* ((table (aws-console/glue/get-table database table))
         (storage-descriptor (cdr (assoc 'StorageDescriptor table)))
         (json-columns (cdr (assoc 'Columns storage-descriptor)))
         (columns (mapcar
                   (lambda (column)
                     (let ((name (cdr (assoc 'Name column)))
                           (type (cdr (assoc 'Type column))))
                       (format "%s:\n%s" name type)))
                   json-columns))
         (column-string (string-join columns "\n\n")))
    (switch-to-buffer (get-buffer-create "*aws-console-glue-schema*"))
    (erase-buffer)
    (insert column-string)))

(defun aws-console/glue/ivy-table-op (database table)
  (ivy-read (format "Operation (%s.%s): " database table)
            '(("Show Schema" aws-console/glue/show-table-schema)
              ("Delete" aws-console/glue/delete-table))
            :action (lambda (selection)
                      (funcall (cadr selection) database table))))

(defun aws-console/glue/ivy-list-tables (database)
  (ivy-read "Table: "
            (aws-console/glue/get-table-names database)
            :sort t
            :action (lambda (table)
                      (aws-console/glue/ivy-table-op database table))))

(defun aws-console/glue/ivy-list-databases ()
  (interactive)
  (ivy-read "Database: "
            (aws-console/glue/get-database-names)
            :sort t
            :action 'aws-console/glue/ivy-list-tables))

(defun aws-console/s3/ls-recursive (bucket key)
  "Expect key to NOT end in a slash. Empty key string for entire bucket"
  (let* ((path (format "s3://%s/%s" bucket key))
         (command (concat "aws s3 ls --recursive " path))
         (output (string-trim (shell-command-to-string command)))
         (entries
          (seq-filter
           (lambda (entry) (> (length entry) 0))
           (split-string output "\n"))))
    (mapcar (lambda (entry)
              (string-match "^[-0-9]+ [:0-9]+\s+[0-9]+ \\(.*\\)" entry)
              (substring (match-string 1 entry) (+ 1 (length key))))
            entries)))

(defun aws-console/s3/open-s3-file (s3-path)
  (let* ((destination "/tmp/emacs-aws-console/")
         (local-file-name
          (progn
            ;; Download the file
            (shell-command (format "aws s3 cp %s %s" s3-path destination))
            ;; Extract it, if it's in a gzip format
            (shell-command (format "gzip -d %s/*gz" destination))
            (string-trim (shell-command-to-string
                          (format "ls %s/ | head -1" destination)))))
         (local-path (concat destination local-file-name)))

    (switch-to-buffer (get-buffer-create "*aws-console-s3-file*"))
    (erase-buffer)

    (message local-path)
    (insert-file-contents local-path)

    (shell-command (format "rm %s" local-path))))

(defun aws-console/s3/ivy-open-file-search (bucket key)
  (let ((entries (aws-console/s3/ls-recursive bucket key)))
    (if entries
        (ivy-read "File to open: "
                  entries
                  :sort t
                  :action (lambda (selection)
                            (aws-console/s3/open-s3-file
                             (format "s3://%s/%s/%s" bucket key selection))))
      (message "No logs to show"))))

(defun aws-console/emr/normalize-cluster-listing (json)
  (let* ((id (cdr (assoc 'Id json)))
         (name (cdr (assoc 'Name json)))
         (status (cdr (assoc 'Status json)))
         (state (cdr (assoc 'State status)))
         (timeline (cdr (assoc 'Timeline status)))
         (created-time (cdr (assoc 'CreationDateTime timeline)))
         (display-name (format "%-13s %s %s" state id name)))
    (list display-name id state created-time)))

(defun aws-console/emr/ls-recent ()
  (let* ((two-days-ago (- (float-time) (* 48 60 60)))
         (two-days-ago-string (format-time-string "%Y-%m-%dT%T" two-days-ago))
         (command (concat "aws emr list-clusters --created-after "
                          two-days-ago-string))
         (output (shell-command-to-string command))
         (json-output (json-read-from-string output))
         (json-cluster-vector (cdr (assoc 'Clusters json-output))))
    (mapcar 'aws-console/emr/normalize-cluster-listing json-cluster-vector)))

(defun aws-console/emr/describe-cluster (cluster-id)
  (let* ((output (shell-command-to-string
                  (concat "aws emr describe-cluster --cluster-id "
                          (shell-quote-argument cluster-id))))
         (json-output (json-read-from-string output)))
    (cdr (assoc 'Cluster json-output))))

(defun aws-console/emr/list-steps (cluster-id)
  (let* ((output (shell-command-to-string
                  (concat "aws emr list-steps --cluster-id "
                          (shell-quote-argument cluster-id))))
         (json-output (json-read-from-string output)))
    (cdr (assoc 'Steps json-output))))

(defun aws-console/emr/get-s3-log-bucket (cluster-id)
  "Returns the bucket,key pair of the s3 bucket log for the given cluster"
  (let* ((cluster-json (aws-console/emr/describe-cluster cluster-id))
         (s3n-uri (cdr (assoc 'LogUri cluster-json))))
    (string-match "^s3n?://\\([-_a-zA-Z0-9]+\\)/\\(.*\\)/$" s3n-uri)
    (list
     (match-string 1 s3n-uri)
     (match-string 2 s3n-uri))))

(defun aws-console/emr/ivy-show-step-logs (cluster-id)
  (let* ((log-bucket (aws-console/emr/get-s3-log-bucket cluster-id))
         (bucket (car log-bucket))
         (log-key (cadr log-bucket))
         (steps-key (format "%s/%s/steps" log-key cluster-id)))
    (aws-console/s3/ivy-open-file-search bucket steps-key)))

(defun aws-console/emr/download-all-logs (cluster-id)
  (let* ((log-bucket (aws-console/emr/get-s3-log-bucket cluster-id))
         (s3-path (format "s3://%s/%s" (car log-bucket) (cadr log-bucket)))
         ;; TODO: Make this configurable, or prompt the user
         (destination "~/Downloads/emrlogs"))
    (shell-command
     (format "aws s3 sync %s %s" s3-path destination))
    (shell-command
     (format "find %s -name '*.gz' -exec gzip -d {} \\;" destination))
    (message (format "Downloaded logs to %s" destination))))

(defun aws-console/emr/ivy-cluster-op (cluster-id)
  (ivy-read (format "Operation (%s): " cluster-id)
            '(("Copy ID to Clipboard" kill-new)
              ("Dump step logs" aws-console/emr/ivy-show-step-logs)
              ("Download all logs" aws-console/emr/download-all-logs))
            :action (lambda (selection)
                      (funcall (cadr selection) cluster-id))))

(defun aws-console/emr/ivy-ls ()
  (interactive)
  (ivy-read "Cluster: "
            (aws-console/emr/ls-recent)
            :sort t
            :action (lambda (selection)
                      (aws-console/emr/ivy-cluster-op (cadr selection)))))

(provide 'aws-console)
