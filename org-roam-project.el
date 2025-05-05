;;; org-roam-project ---

;;; Commentary:
;; Load this package with:
;; (require 'org-roam-project)

;;; Code:

;; (org-roam-db-query
;;      [:select [nodes:file]
;;       :from tags
;;       :left-join nodes
;;       :on (= tags:node-id nodes:id)
;;       :where (like tag (quote "%\"project\"%"))])

(require 'org-roam)
(require 'org-roam-dailies)

(defvar org-roam-project-keywords
  '(("STATE" . "u") "SCHEDULE_START" "SCHEDULE_END" "REPEAT" "EFFORT"))
(defvar org-roam-project-states '("TODO" "RUN" "DONE"))
(defvar org-roam-project-tag "project")
(defvar org-roam-project-link-property "SUBTASK")
(defvar org-roam-project-clock-property "CLOCK")
(defvar org-roam-project-link-heading "Subtasks")
(defvar org-roam-project-clock-heading "Clocking")
(defvar org-roam-project-capture-templates nil)

(defun org-roam-project-node-p ( node)
  (when (and node
             (member org-roam-project-tag
                     (org-roam-node-tags node)))
    node))

(defun org-roam-project-node-from-id ( id)
  "Given an Org Roam ID return the corresponding project node.

ID is the Org Roam unique identifier.  If no such node exists,
nil is returned."
  (org-roam-project-node-p (org-roam-node-from-id id)))

(defun org-roam-project-file-p ()
  (and (org-roam-file-p)
       (save-excursion
         (goto-char (point-min))
         (and (re-search-forward "#\\+filetags:" nil t)
              (re-search-forward ":project:" (line-end-position) t)
              (org-roam-node-at-point)))))

(defun org-roam-project-node-effort ( node)
  (or (cdr (assoc "EFFORT" (org-roam-node-properties node)))
      (user-error (concat "File " (org-roam-node-file node) " has no effort."))))

(defun org-roam-project-node-state ( node)
  (cdr (assoc "STATE" (org-roam-node-properties node))))

(defun org-roam-project-node-clock ( node)
  (let (( raw-clocks (cdr (assoc "CLOCK" (org-roam-node-properties node))))
        clocks)
    (when raw-clocks
      (dolist ( raw-clock (split-string raw-clocks))
        (setq clocks (cons (replace-regexp-in-string "_" " " raw-clock) clocks))))
      (nreverse clocks)))

(defun org-roam-project-remove-prop ( key)
  (save-excursion
    (goto-char (point-min))
    (let (( limit (save-excursion (re-search-forward "^*+\s" nil t))))
      (while (re-search-forward (concat ":" key "\\+?:") limit t)
        (delete-region (line-beginning-position) (line-beginning-position 2))))))

(defun org-roam-project-set-keyword ( key value)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "#\\+" key ":\s\\(.*?\\)$") nil t)
        (replace-match value t nil nil 1)
      (re-search-forward org-property-end-re nil t)
      (while (re-search-forward "#\\+.*?\n" nil t))
      (insert "#+" key ": " value))))

(defun org-roam-project-links-under-heading ( heading)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\*+\s+" heading "\s*$") nil t)
      (let (( end (save-excursion
                    (or (outline-get-next-sibling) (point))))
            nodes)
        (while (re-search-forward org-link-bracket-re end t)
          (when (string-match-p "^id:" (match-string-no-properties 1))
            (setq nodes (cons (org-roam-node-from-id
                             (replace-regexp-in-string
                             "^id:" "" (match-string-no-properties 1)))
                            nodes))))
        (nreverse nodes)))))

(defun org-roam-project-write-props ( key values)
  (goto-char (point-min))
  (when (re-search-forward org-property-end-re nil t)
    (beginning-of-line)
    (dolist ( value values)
              (insert ":" key "+: " value "\n"))))

(defun org-roam-project-collect-links ()
  (save-excursion
    (org-roam-project-remove-prop org-roam-project-link-property)
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\*+\s+"
                                   org-roam-project-link-heading
                                   "\s*$")
                           nil t)
        (let (( end (save-excursion
                      (or (outline-get-next-sibling) (point))))
              ids)
          (while (re-search-forward org-link-bracket-re end t)
            (when (string-match-p "^id:" (match-string-no-properties 1))
              (setq ids (cons (replace-regexp-in-string
                               "^id:" "" (match-string-no-properties 1))
                              ids))))
          (org-roam-project-write-props org-roam-project-link-property
                                        (nreverse ids))))))

(defun org-roam-project-collect-clocks ()
  (save-excursion
    (org-roam-project-remove-prop org-roam-project-clock-property)
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\*+\s+"
                                     org-roam-project-clock-heading
                                     "\s*$")
                             nil t)
      (let (( limit (save-excursion
                      (or (outline-get-next-sibling) (point))))
            clocks)
        (while (re-search-forward org-logbook-drawer-re limit t)
          (goto-char (match-beginning 0))
          (end-of-line)
          (let (( logbook-end (match-end 0)))
            (while (re-search-forward "^\s*CLOCK:\s*\\(\\[.*\\]\\)" logbook-end t)
              (setq clocks (cons (replace-regexp-in-string
                                  "\s" "_"
                                  (match-string-no-properties 1))
                                 clocks)))))
        (org-roam-project-write-props org-roam-project-clock-property
                                      (nreverse clocks))))))

(defun org-roam-project-backlink-nodes ( node)
  (let ( nodes)
    (dolist ( backlink (org-roam-backlinks-get node))
      (let* (( bl-node (org-roam-backlink-source-node backlink))
             ( bl-subids (org-roam-project-node-subids bl-node)))
        (when (and (org-roam-project-node-p bl-node)
                   (member (org-roam-node-id node) bl-subids))
          (setq nodes (cons bl-node nodes)))))
    nodes))

(defun org-roam-project-task-props ()
  "Write content to properties."
  (org-roam-project-collect-links)
  (org-roam-project-collect-clocks)
  (save-excursion
    (goto-char (point-min))
    (let (( keywords (org-collect-keywords
                      (mapcar (lambda (key) (if (consp key) (car key) key))
                              org-roam-project-keywords))))
      (dolist ( keyword keywords)
        (let* (( prop (car keyword))
               ( value (cadr keyword))
               ( flag (cdr (assoc prop org-roam-project-keywords))))
          (org-roam-project-remove-prop prop)
          (unless (string-empty-p value)
            (cond ((string= flag "u")
                   (setq value (upcase value))))
            (org-set-property prop value))))))
  (org-fold-hide-drawer-all))

(defun org-roam-project-before-save ()
  (cond ((org-roam-project-file-p)
         (org-roam-project-task-props))
        ((org-roam-project-daily-file-time)
         (org-roam-project-collect-links))))

(add-hook 'before-save-hook 'org-roam-project-before-save)

(defun org-roam-project-nodes ()
  (mapcar (lambda ( item) (org-roam-node-from-id (car item)))
          (org-roam-db-query [:select [nodes:id]
                              :from nodes
                              :left-join tags
                              :on (= nodes:id tags:node-id)
                              :where (= tags:tag $s1)]
                     org-roam-project-tag)))

(defun org-roam-project-id ( task)
  (car task))

(defun org-roam-project-root-nodes ( project-nodes)
  (let ( nodes)
    (dolist ( node project-nodes)
      (unless (org-roam-project-backlink-nodes node)
        (setq nodes (cons node nodes))))
    nodes))

(defun org-roam-project-node-subids ( node)
  (when node
    (let (( ids-string (cdr (assoc org-roam-project-link-property
                                   (org-roam-node-properties node)))))
      (when ids-string
        (split-string ids-string)))))

(defun org-roam-project-subnodes ( node)
  (let (( ids-string (cdr (assoc org-roam-project-link-property
                                 (when node (org-roam-node-properties node)))))
        nodes)
    (when ids-string
      (mapcar (lambda (id) (org-roam-node-from-id id))
              (split-string ids-string)))))

(defun org-roam-project-preorder ( root-nodes)
  (setq root-nodes (mapcar 'list root-nodes))
  (let ( project-trees)
    (dolist ( q root-nodes)
      (let (( level 1)
            res node-level)
        (setq q (list `(,(car q) . ,level)))
        (while q
          (setq node-level (pop q)
                res (cons `(,(car node-level) . ,(cdr node-level)) res)
                level (1+ (cdr node-level))
                q (append (mapcar (lambda (subnode) `(,subnode . ,level))
                                  (org-roam-project-subnodes (car node-level)))
                          q)))
        (setq project-trees (cons (nreverse res) project-trees))))
    project-trees))

(defun org-roam-project-goals-tasks ( root-nodes)
  (let ( goals tasks)
    (dolist ( tree (org-roam-project-preorder root-nodes))
      (if (> (length tree) 1)
          (setq goals (cons tree goals))
        (setq tasks (cons (caar tree) tasks))))
    `(,goals . ,tasks)))

(defun org-roam-project-columnview-defs ( item-name)
  (org-set-property "COLUMNS" (concat "%35ITEM(" item-name ") "
          "%EFFORT{:}(Effort) %CLOCKSUM(Clock) %STATE(State)")))

(defun org-roam-project-columnview-block ( id props)
  (insert (if (= (point) (line-beginning-position)) "" "\n")
          "#+BEGIN: columnview "
          (string-trim (format "%s" props) "(" ")")
          " :id " id "\n"
          "#+END:\n"))

(defun org-roam-project-heading ( level heading &optional base-level separate)
  (unless (= (point) (line-beginning-position))
    (insert "\n"))
  (let (( net-level (+ level (or base-level 0))))
    (insert (if separate "\n" "")
            (if (= net-level 1) "\n" "")
            (make-string net-level ?*) " "
            heading)))

(defun org-roam-project-set-effort ( effort)
  "Silently set effort for current heading.

EFFORT is Org Mode duration time string."
  (let (( inhibit-message t)
        (message-log-max nil))
    (org-set-effort nil effort)))

(defun org-roam-project-tree-goals ( goals &optional item-name time)
  (org-roam-project-heading 1 "Goals")
  (org-roam-project-columnview-defs (or item-name "Goal"))
  (org-end-of-subtree)
  (org-roam-project-columnview-block "local"
                                     '(:hlines 2 :skip-empty-rows "t" :indent "t"))
  (let (( project-num 0))
    (dolist (project goals)
      (setq project-num (1+ project-num))
      (dolist ( node-level project)
        (let* (( level (1+ (cdr node-level)))
               ( node (car node-level))
               ( id (org-roam-node-id node))
               ( title (org-roam-node-title node))
               ( link (org-link-make-string (concat "id:" id) title))
               ( effort (org-roam-project-node-effort node))
               ( clocks (org-roam-project-node-clock node)))
          (org-roam-project-heading level link)
          (org-roam-project-set-effort effort)
          (org-end-of-subtree)
          (org-roam-project-log-clocks clocks time)
          (insert "\n"))))))

(defun org-roam-project-tree-tasks ( nodes &optional item-name time)
  (org-roam-project-heading 1 "Tasks")
  (org-roam-project-columnview-defs (or item-name "Task"))
  (org-end-of-subtree)
  (org-roam-project-columnview-block "local"
                                     '(:hlines 1 :skip-empty-rows "t" :indent "t"))
  (dolist ( node nodes)
    (let* (( id (org-roam-node-id node))
           ( title (org-roam-node-title node))
           ( link (org-link-make-string (concat "id:" id) title))
           ( effort (org-roam-project-node-effort node))
           ( state (org-roam-project-node-state node))
           ( clocks (org-roam-project-node-clock node)))
      (org-roam-project-heading 1 link 1)
      (org-roam-project-set-effort effort)
      (when state
        (org-set-property "STATE" state))
      (org-end-of-subtree)
      (org-roam-project-log-clocks clocks time)
      (insert "\n"))))

(defun org-roam-project-goal-tasks ( goals)
  (let ( tasks)
    (dolist ( goal goals)
      (dolist ( node-level goal)
        (let (( node (car node-level)))
          (unless (org-roam-project-subnodes node)
            (setq tasks (cons node tasks))))))
    tasks))

(defun org-roam-project-daily-time ( &optional read-time)
  (org-roam-project-string-to-time
   (if read-time
       (car (string-split (org-read-date)))
     (or (org-roam-project-daily-time-string)
         (format-time-string "%Y-%m-%d" (current-time))))))

(defun org-roam-project-daily-root-nodes ( &optional time)
  "Return subnodes for a Org Roam Dailies node.

TIME specifies the dailies node.  If nil is given, try to read the
dailies node for the current day.  If such node does not exist,
return nil."
  (org-roam-project-subnodes (org-roam-project-daily-node-time time)))

(defun org-roam-project-buffer ( &optional arg)
  (interactive "P")
  (let* (( time (org-roam-project-daily-time (equal arg '(4))))
         ( daily-node (org-roam-project-daily-node-time time))
         ( daily-link (if daily-node
                          (org-link-make-string
                           (concat "id:" (org-roam-node-id daily-node))
                           (org-roam-node-title daily-node))
                        (org-link-make-string
                         (concat "dailies:" (format-time-string "%Y-%m-%d" time))
                         (format-time-string "%A, %d. %B %Y" time))))
         ( root-nodes-time (org-roam-project-subnodes daily-node))
         ( root-nodes (org-roam-project-root-nodes (org-roam-project-nodes))))
    (switch-to-buffer-other-window "*Org Roam Project*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (insert "#+title: Org Roam Project\n")
    (let* (( goals-tasks (org-roam-project-goals-tasks root-nodes-time))
           ( goals (car goals-tasks))
           ( goal-tasks (org-roam-project-goal-tasks goals))
           ( tasks (cdr goals-tasks)))
      (org-roam-project-tree-tasks (append tasks goal-tasks)
                                   (concat "Tasks " daily-link) time)
      (when goals
        (org-roam-project-tree-goals goals "Goals" time)))
    (let* (( goals-tasks (org-roam-project-goals-tasks root-nodes))
           ( goals (car goals-tasks))
           ( goal-tasks (org-roam-project-goal-tasks goals))
           ( tasks (cdr goals-tasks)))
      (org-roam-project-tree-goals goals "Goals All")
      (org-roam-project-tree-tasks goal-tasks "Goal Tasks Summary")
      (org-roam-project-tree-tasks tasks "Non-Goal Tasks")
      (org-roam-project-tree-tasks (append tasks goal-tasks) "Tasks All"))
    (org-update-all-dblocks)
    (org-roam-project-buffer-clean-up
     (org-roam-project-buffer-today-p time)))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun org-roam-project-buffer-today-p ( time)
  (string= (format-time-string "%Y-%m-%d" time)
           (format-time-string "%Y-%m-%d" (current-time))))

(defun org-roam-project-buffer-clean-up ( &optional goto-run)
  (goto-char (point-max))
  (beginning-of-line)
  (unless (looking-at "|\\|#\\+")
    (delete-region (point) (line-end-position)))
  (while (> (point) (point-min))
    (beginning-of-line 0)
    (unless (looking-at "|\\|#\\+")
      (delete-region (point) (line-beginning-position 2))))
  (while (re-search-forward "#\\+begin:.*?\n\\|#\\+end:" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (when goto-run
    (re-search-forward (concat "|\s*" (cadr org-roam-project-states) "\s*|")
                       nil t))
  (beginning-of-line))

(defun org-roam-project-link-at-point ()
  "Return org-roam-project node of link at point."
  (when (and (org-in-regexp org-link-bracket-re)
             (string-match-p "^id:" (match-string-no-properties 1)))
    (org-roam-project-node-from-id (replace-regexp-in-string
                            "^id:" "" (match-string-no-properties 1)))))

(defun org-roam-project-file-clock-in ()
  (save-excursion
    (let* (( keywords (org-collect-keywords '("title" "effort")))
           ( title (cadr (assoc "TITLE" keywords)))
           ( effort (cadr (assoc "EFFORT" keywords))))
      (goto-char (point-min))
      (unless (re-search-forward (concat "^\\*+\s+"
                                         org-roam-project-clock-heading)
                                 nil t)
        (goto-char (point-max))
        (insert "\n\n* " org-roam-project-clock-heading))
      (unless (re-search-forward (concat "^\\*\\*+\s+" title)
                                 (save-excursion (or (outline-get-next-sibling)
                                                     (point)))
                                 t)
        (insert "\n** " title))
      (org-roam-project-set-effort effort)
      (org-clock-in)
      `(,(org-roam-id-at-point) . ,title))))

(defun org-roam-project-clock-in ()
  (interactive)
  (let ( node id-title)
    (if (setq node (org-roam-project-link-at-point))
        (with-current-buffer (find-file-noselect (org-roam-node-file node))
          (setq id-title (org-roam-project-file-clock-in)))
      (when (setq node (org-roam-project-file-p))
        (setq id-title (org-roam-project-file-clock-in))))
    (unless node
        (user-error "No Org Roam Project Node clocked-in."))
    (org-roam-project-daily-add (car id-title) (cdr id-title))
    (message "%s clocked-in." (cdr id-title))))

(defun org-roam-project-running-clock ()
  (unless (re-search-forward "\\]--\\[" (line-beginning-position) t -1)
    (org-insert-timestamp (current-time) 'with-hm 'inactive "--")
    (org-set-property "STATE" (cadr org-roam-project-states)))
  (end-of-line))

(defun org-roam-project-clock-string ( &optional time time-end)
  (if time-end
      (concat (format-time-string (org-time-stamp-format t t) time) "--"
              (format-time-string (org-time-stamp-format t t) time-end))
    (format-time-string (org-time-stamp-format t t) time)))

(defun org-roam-project-clock-filter ( clock time-start &optional time-end)
  (setq time-end (or time-end (time-add time-start 86400)))
  (when (time-less-p (current-time) time-end)
    (setq time-end (current-time)))
  (let (( start 0)
        in out)
    (when (string-match "\\[.*?\\]" clock start)
      (setq in (org-time-string-to-time (match-string-no-properties 0 clock))
            start (match-end 0))
      (when (string-match "\\[.*?\\]" clock start)
        (setq out (org-time-string-to-time (match-string-no-properties 0 clock))))
      (cond ((or (time-less-p time-end in) (time-less-p out time-start))
             nil)
            ((and out (time-less-p time-start in) (time-less-p time-end out))
             (org-roam-project-clock-string in time-end))
            ((and out (time-less-p in time-start) (time-less-p out time-end))
             (org-roam-project-clock-string time-start out))
            ((and out (time-less-p in time-start) (time-less-p time-end out))
             (org-roam-project-clock-string time-start time-end))
            ((and (not out) (time-less-p in time-start))
             (org-roam-project-clock-string time-start))
            (t
             clock)))))

(defun org-roam-project-log-clocks ( clocks &optional time)
  "Create logbook drawer and populate with clocking lines.

CLOCKS is a list containing clock lines of the form

[2025-04-29 Di 17:02]--[2025-04-29 Di 17:10].

Duration is added by `org-evaluate-time-range' for each clock line.

The optional argument TIME functions as a filter for clock lines."
  (when clocks
    (insert "\n:LOGBOOK:")
    (dolist ( clock clocks)
      (when time
        (setq clock (org-roam-project-clock-filter clock time)))
      (when clock
        (insert "\nCLOCK: " clock)
        (org-roam-project-running-clock)
        (org-evaluate-time-range)
        (end-of-line)))
    (insert "\n:END:\n")))

(setq org-roam-project-capture-templates
      '(("w" "internal" plain ""
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%A, %d. %B %Y>\n")
         :unnarrowed t
         :jump-to-captured nil
         :kill-buffer nil ; don't kill base buffer after capture
         :immediate-finish nil ; prevent finishing calling captures
         :no-save nil))) ; save after capture

(defun org-roam-project-daily-buffer ( &optional time)
  (let (( buffer-exists (get-buffer (format-time-string "%Y-%m-%d.org" time)))
        ( capture-plist-save org-capture-plist)
        ( org-roam-directory (expand-file-name org-roam-dailies-directory
                                               org-roam-directory))
        ( org-roam-dailies-directory "./")
        ( immediate-finish (plist-get
                            (assoc "w" org-roam-project-capture-templates)
                            :immediate-finish)))
    (org-roam-capture- :goto nil
                       :keys "w"
                       :node (org-roam-node-create)
                       :templates org-roam-project-capture-templates
                       :props (list :override-default-time
                                    (or time (current-time))))
    ;; (run-hooks 'org-roam-dailies-find-file-hook)
    (setq org-capture-plist capture-plist-save)
    (let (( base-buffer (buffer-base-buffer (current-buffer))))
      (kill-buffer (current-buffer))
      `(,base-buffer . ,buffer-exists))))

(defun org-roam-project-daily-node ( time-string)
  "Get Org Roam Dailies node by a time string.

TIME-STRING should have the format %Y-%m-%d, e.g. 2025-05-02."
  (org-roam-node-from-id
   (caar (org-roam-db-query
          [:select [id]
	               :from nodes
	               :where (like properties $r1)]
          (concat "%%\"CATEGORY\" . \"" time-string "\"%%")))))

(defun org-roam-project-daily-node-time ( &optional time)
  "Get Org Roam Dailies node by a Lisp timestamp.

TIME is a Lisp timestamp or nil.  In case of nil, the current time
is used.  If no dailies node exists for the given time, nil is returned."
  (org-roam-project-daily-node
   (format-time-string "%Y-%m-%d" (or time (current-time)))))

(defun org-roam-project-daily-add ( id title &optional time)
  "Add a link to an org-roam-project task at org-roam-dailies node.

ID and TITLE refers to an org-roam-project task node.  LISP-TIMESTAMP can
be used to add the link at a specific date."
  (if (member id (org-roam-project-node-subids
                  (org-roam-project-daily-node-time time)))
      (message "%s already scheduled for today." title)
    (let* (( buffers (org-roam-project-daily-buffer time))
           ( base-buffer (car buffers))
           ( buffer-exists (cdr buffers)))
      (with-current-buffer base-buffer
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\*\s+" org-roam-project-link-heading)
                               nil t)
            (org-end-of-subtree)
          (goto-char (point-max))
          (insert "\n\n* " org-roam-project-link-heading))
        (insert "\n- " (org-link-make-string (concat "id:" id) title))
        (save-buffer)
        (unless buffer-exists
          (kill-buffer))))))

(defun org-roam-project-daily-time-string ()
  (when (and buffer-file-name
             (org-roam-dailies--daily-note-p))
    (file-name-base (buffer-file-name))))

(defun org-roam-project-string-to-time ( time-string)
  (encode-time (decoded-time-set-defaults (parse-time-string time-string))))

(defun org-roam-project-daily-file-time ()
  (let (( time-string (org-roam-project-daily-time-string)))
    (when time-string
      (org-roam-project-string-to-time time-string))))

(defun org-roam-project-new-node-clock-in ()
  (when (org-roam-project-file-p)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "#\\+capture-flag:\s+\\([^ ]*\\)\s*\n" nil t)
        (let (( flag (match-string-no-properties 1)))
          (replace-match "")
          (cond ((string= flag "clock-in")
                 (org-roam-project-clock-in))
                ((string= flag "time-prompt")
                 (let* (( id (org-roam-id-at-point))
                        ( title (cadar (org-collect-keywords '("title"))))
                        ( time (org-time-string-to-time
                                (with-temp-buffer
                                  (org-time-stamp nil)))))
                   (org-roam-project-daily-add id title time)
                   (message "12 %s" (marker-buffer  org-capture-last-stored-marker))
                   (message "%s %s" (org-roam-capture--get :new-file)
                            (find-buffer-visiting (org-roam-capture--get :new-file))
                   (message "Added %s to dailies at %s."
                            title (format-time-string "%Y-%m-%d" time)))))))
      ))))

(add-hook 'org-roam-capture-new-node-hook 'org-roam-project-new-node-clock-in)

(org-link-set-parameters
 "dailies"
 :follow 'org-roam-project-daily-follow-link
 :face 'org-link)

(defun org-roam-project-daily-follow-link ( path arg)
  (let (( time (org-roam-project-string-to-time path))
        ( org-roam-directory (expand-file-name org-roam-dailies-directory
                                               org-roam-directory))
        ( org-roam-dailies-directory "./"))
    (org-roam-capture- :goto '(4) ;; nil leads to capture buffer
                       :keys "d"
                       :node (org-roam-node-create)
                       :templates org-roam-dailies-capture-templates
                       :props (list :override-default-time time))
    (run-hooks 'org-roam-dailies-find-file-hook)))

(provide 'org-roam-project)

;;; org-roam-project.el ends here
