;;; org-roam-project ---  -*- lexical-binding: t; -*-

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
(require 'ivy)

(defvar org-roam-project-keywords
  '(("STATE" . "u") "SCHEDULE_START" "SCHEDULE_END" "REPEAT" "EFFORT"))
(defvar org-roam-project-run-id "RUN")
(defvar org-roam-project-tag "project")
(defvar org-roam-project-link-property "SUBTASK")
(defvar org-roam-project-clock-property "CLOCK")
(defvar org-roam-project-done-property "DONE")
(defvar org-roam-project-link-heading "Subtasks")
(defvar org-roam-project-clock-heading "Clocking")
(defvar org-roam-project-capture-templates nil)
(defvar org-roam-project-buffer-name "*Org Roam Project*")
(defvar org-roam-project-update-link-title t)

(defun org-roam-project-node-p ( node &optional user-error update-db)
  (cond ((and node
              (org-roam-project-node-effort node))
         (when update-db
           (with-current-buffer (find-file-noselect (org-roam-node-file node))
             (save-buffer)))
         node)
        (t
         (when user-error
           (user-error "Not a Org-Roam-Project node")))))

(defun org-roam-project-node-from-id ( id &optional update-db)
  "Given an Org Roam ID return the corresponding project node.

ID is the Org Roam unique identifier.  If no such node exists,
nil is returned."
  (org-roam-project-node-p (org-roam-node-from-id id)
                           update-db))

(defun org-roam-project-file-p ()
  (and (org-roam-file-p)
       (save-excursion
         (goto-char (point-min))
         (and (re-search-forward "#\\+effort:" nil t)
              (org-roam-node-at-point)))))

(defun org-roam-project-node-effort ( node)
  (cdr (assoc "EFFORT" (org-roam-node-properties node))))

(defun org-roam-project-node-state ( node)
  (cdr (assoc "STATE" (org-roam-node-properties node))))

(defun org-roam-project-node-clock ( node)
  (let (( raw-clocks (cdr (assoc org-roam-project-clock-property
                                 (org-roam-node-properties node))))
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

;; (defun org-roam-project-set-keyword ( key value)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (re-search-forward (concat "#\\+" key ":\s\\(.*?\\)$") nil t)
;;         (replace-match value t nil nil 1)
;;       (re-search-forward org-property-end-re nil t)
;;       (while (re-search-forward "#\\+.*?\n" nil t))
;;       (insert "#+" key ": " value))))

(defun org-roam-project-subnodes-re ( heading)
  "Return regexp for a heading line.

HEADING is the title without any doto or cooky fields.  Currently
no tags are allowed.  The todo state is saved in match number
2. Cookies are placed in match number 3."
  (concat "\\*+\s+\\(\\(TODO\\|DONE\\)\s*\\)?\\(\\[.*]\s+\\)?" heading "\s*$"))

(defun org-roam-project-next-link-id ( &optional limit backward)
  "Jump to next roam link and return the target id.

Match data is preserved, i.e. group 1 contains the link id and
group 2 the description.  LIMIT and BACKWARD is doing
pretty much what one expects."
  (when (re-search-forward org-link-bracket-re limit t (and backward -1))
    (when (string-match-p "^id:" (match-string-no-properties 1))
      (replace-regexp-in-string "^id:" "" (match-string-no-properties 1)))))

(defun org-roam-project-next-link ( &optional limit backward update-db)
  "Jump to next roam link and return the target node.

Match data is preserved, i.e. group 1 contains the link id and
group 2 the description.  LIMIT BACKWARD and UPDATE-DB is doing
pretty much what one expects."
  (org-roam-project-node-from-id (org-roam-project-next-link-id limit backward)
                                 update-db))
    
(defun org-roam-project-ids-under-heading ( heading)
  (save-excursion
    (let (( end (cdr (org-roam-project-get-heading heading)))
          ids)
      (when end
        (while (re-search-forward org-link-bracket-re end t)
          (when (string-match-p "^id:" (match-string-no-properties 1))
            (setq ids (cons (replace-regexp-in-string
                             "^id:" "" (match-string-no-properties 1))
                            ids))))
        (nreverse ids)))))

(defun org-roam-project-links-under-heading ( heading)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (org-roam-project-subnodes-re heading) nil t)
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

(defun org-roam-project-subtree ( heading)
  (goto-char (point-min))
  (when (re-search-forward (org-roam-project-subnodes-re heading) nil t)
    `(,(line-beginning-position) .
      ,(save-excursion
         (or (outline-get-next-sibling) (point))))))

(defun org-roam-project-get-heading ( heading)
  "Goto beginning of heading and return section limits.

HEADING is the plain heading title without cookies.  When heading
is found, leave point at the beginning of headline."
  (goto-char (point-min))
  (when (re-search-forward (org-roam-project-subnodes-re heading) nil t)
    (beginning-of-line)
    `(,(point) . ,(save-excursion
                    (save-match-data
                      (or (outline-get-next-sibling) (point)))))))

(defun org-roam-project-read-time ()
  "Use convenient calendar prompt for reading in a lisp-timestamp."
  (let* (( time-string (with-temp-buffer (org-time-stamp nil)))
         ( day-time (org-roam-project-has-time-p time-string))
         ( char (when day-time
                  (char-to-string (read-char-choice
                   "Day-Time Key: Plain (p), Scheduled (s), Deadline (d), Quit (q)? "
                   '("p" "s" "d" "q")))))
         ( day-time-flag (cond ((string= char "p")
                                day-time)
                               ((string= char "s")
                                (concat day-time ">"))
                               ((string= char "d")
                                (concat "<" day-time))
                               (t
                                ""))))
    `(,(org-time-string-to-time time-string) .
      ,day-time-flag)))

(defun org-roam-project-collect-links ()
  (save-excursion
    (org-roam-project-remove-prop org-roam-project-link-property)
    (let (( end (cdr (org-roam-project-subtree org-roam-project-link-heading)))
          ids)
      (when end
        (while (re-search-forward org-link-bracket-re end t)
          (when (string-match-p "^id:" (match-string-no-properties 1))
            (setq ids (cons (replace-regexp-in-string
                             "^id:" "" (match-string-no-properties 1))
                            ids))))
        (org-roam-project-write-props org-roam-project-link-property
                                      (delete-dups (nreverse ids)))))))

(defun org-roam-project-collect-clocks ()
  (save-excursion
    (org-roam-project-remove-prop org-roam-project-clock-property)
    (goto-char (point-min))
    (when (re-search-forward (org-roam-project-subnodes-re
                              org-roam-project-clock-heading)
                             nil t)
      (let (( limit (save-excursion
                      (or (outline-get-next-sibling) (point))))
            ( done "")
            clocks)
        (while (re-search-forward org-logbook-drawer-re limit t)
          (goto-char (match-beginning 0))
          (end-of-line)
          (let (( logbook-end (match-end 0)))
            (while (re-search-forward "^\s*CLOCK:\s*\\(\\[.*\\]\\)\\|^\s*\\(DONE\\)"
                                      logbook-end t)
              (if (match-string-no-properties 2)
                  (setq done "DONE")
                (setq clocks (cons (concat (replace-regexp-in-string
                                            "\s" "_"
                                            (match-string-no-properties 1))
                                           done)
                                   clocks)
                      done "")))))
        (org-roam-project-write-props org-roam-project-clock-property
                                      (nreverse clocks))))))

(defun org-roam-project-backlink-nodes ( node)
  "Return all backlinks from project parent nodes (goals).

NODE is a valid org-roam node.  Returned superior node list has a
depth of 1."
  (let ( nodes)
    (dolist ( backlink (org-roam-backlinks-get node))
      (let* (( bl-node (org-roam-backlink-source-node backlink))
             ( bl-subids (org-roam-project-node-subids bl-node)))
        (when (and (org-roam-project-node-p bl-node)
                   (member (org-roam-node-id node) bl-subids))
          (setq nodes (cons bl-node nodes)))))
    nodes))

(defun org-roam-project-node-siblings ( node)
  (let (( bl-nodes (org-roam-project-backlink-nodes node))
        siblings)
    (dolist ( bl-node bl-nodes)
      (setq siblings (append (org-roam-project-subnodes bl-node) siblings)))
    siblings))

(defun org-roam-project-goals-buffer ()
  (interactive)
  (let* (( child-node (org-roam-project-node-at-point))
         ( child-id (org-roam-node-id child-node))
         ( nodes-roots (org-roam-project-preorder-reverse child-node))
         ( ids (mapcar 'org-roam-node-id (car nodes-roots)))
         ( sibling-ids (mapcar 'org-roam-node-id
                               (org-roam-project-node-siblings child-node)))
         ( roots (cdr nodes-roots))
         ( trees (org-roam-project-preorder roots)))
    (switch-to-buffer-other-window "*Org-Roam Goals*")
    (erase-buffer)
    (org-mode)
    (insert "#+title: Goals Buffer\n\n"
            (propertize "Goals reached by: " 'font-lock-face '(:foreground "deep pink"))
            (org-link-make-string (concat "id:" child-id)
                                  (org-roam-node-title child-node))
            "\n")
    (dolist ( tree trees)
      (dolist ( node-level tree)
        (let* (( node (car node-level))
               ( id (org-roam-node-id node))
               ( done (org-roam-project-done-p node))
               ( level (cdr node-level))
               sibling-p)
          (when (or (setq sibling-p (member id sibling-ids))
                    (member id ids))
            (insert (if sibling-p
                        (concat (make-string level ? ) "- "
                                (if done "[X] " "[ ] "))
                      (concat "\n" (make-string level ?*) " TODO [/][%] "))
                    (org-link-make-string (concat "id:" id)
                                          (org-roam-node-title node))
                    (if sibling-p "\n" "\n\n"))
            (when (string= id child-id)
              (let (( overlay (make-overlay (line-beginning-position 0)
                                            (line-beginning-position))))
                (overlay-put overlay 'face '(:background "LightPink1" :extend t))
                (overlay-put overlay 'name "org-roam-project"))))))))
  (org-update-checkbox-count)
  (goto-char (point-min)))

(defun org-roam-project-task-props ()
  "Write content to properties."
  (org-roam-project-collect-links)
  (org-roam-project-collect-clocks)
  (save-excursion
    (goto-char (point-min))
    (let* (( keys (mapcar (lambda (key) (if (consp key) (car key) key))
                              org-roam-project-keywords))
           ( keywords (org-collect-keywords keys)))
      (dolist ( key keys)
        (org-roam-project-remove-prop key))
      (dolist ( keyword keywords)
        (let* (( prop (car keyword))
               ( value (cadr keyword))
               ( flag (cdr (assoc prop org-roam-project-keywords))))
          (unless (string-empty-p value)
            (cond ((string= flag "u")
                   (setq value (upcase value))))
            (org-set-property prop value))))))
  (org-fold-hide-drawer-all))

(defun org-roam-project-before-save ()
  (cond ((org-roam-project-file-p)
         (org-roam-project-task-props))
        ((org-roam-project-daily-file-time)
         (org-roam-project-spread-goal)
         (org-roam-project-collect-links)
         (org-roam-project-format-all)
         (org-roam-project-daily-stats)))
  (when (or (org-roam-project-file-p) (org-roam-project-daily-file-time))
    (org-update-checkbox-count)
    (when org-roam-project-update-link-title
      (org-roam-project-links-title-update))))

(defun org-roam-project-after-save ()
  (when (and (or (org-roam-project-file-p)
                 (org-roam-project-daily-file-time))
             (get-buffer-window org-roam-project-buffer-name))
    (org-roam-project-buffer)))

(add-hook 'before-save-hook 'org-roam-project-before-save)
(add-hook 'after-save-hook 'org-roam-project-after-save)

(defun org-roam-project-nodes-old ()
  "Get all Org Roam Project nodes."
  (mapcar (lambda ( item) (org-roam-node-from-id (car item)))
          (org-roam-db-query [:select [nodes:id]
                                      :from nodes
                                      :left-join tags
                                      :on (= nodes:id tags:node-id)
                                      :where (= tags:tag $s1)]
                             org-roam-project-tag)))

(defun org-roam-project-nodes ()
  "Get all Org Roam Project nodes.

By definition, a org-roam-project node has an effort property."
  (mapcar (lambda ( item) (org-roam-node-from-id (car item)))
          (org-roam-db-query
           [:select [id]
	                :from nodes
	                :where (like properties $r1)]
           (concat "%%\"EFFORT\"%%"))))

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

(defun org-roam-project-subnodes ( node &optional properties)
  (let (( ids-string (cdr (assoc org-roam-project-link-property
                                 (or properties
                                     (when node
                                       (org-roam-node-properties node)))))))
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

(defun org-roam-project-preorder-reverse ( node)
  (let (( q (list node))
        res res-roots bls bl-node)
    (while q
      (setq bl-node (pop q)
            bls (org-roam-project-backlink-nodes bl-node)
            res (cons bl-node res)
            res-roots (if bls res-roots (cons bl-node res-roots))
            q (append (org-roam-project-backlink-nodes bl-node) q)))
    `(,res . ,res-roots)))

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
  "Given parent nodes (goals), get corresponding daily tasks.

GOALS can be a single parent node (goal or subgoal) or a list of
goals.  If it is a list, the return of duplicate nodes is
possible."
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
         (format-time-string "%Y-%m-%d")))))

(defun org-roam-project-daily-root-nodes ( &optional time)
  "Return subnodes for a Org Roam Dailies node.

TIME specifies the dailies node.  If nil is given, try to read the
dailies node for the current day.  If such node does not exist,
return nil."
  (org-roam-project-subnodes (org-roam-project-daily-node-time time)))

(defun org-roam-project-buffer-display ()
  (if (get-buffer-window org-roam-project-buffer-name)
      (get-buffer org-roam-project-buffer-name)
    (if (or (org-roam-project-file-p)
            (org-roam-project-daily-file-time))
        (switch-to-buffer-other-window org-roam-project-buffer-name)
      (switch-to-buffer org-roam-project-buffer-name))))

(defun org-roam-project-duration-to-minutes ( duration)
  (if (string-match "^\s*-" duration)
      0
    (org-duration-to-minutes (string-trim duration "+"))))

(defun org-roam-project-sign-prefix ( string)
  (concat (when (string-match "^[^-]" string) "+") string))

(defun org-roam-project-daily-stats ()
  (when (org-roam-project-daily-time-string)
    (let* (( end (cdr (org-roam-project-subtree org-roam-project-link-heading)))
           ( sum-todo-minutes 0)
           ( sum-effort-minutes 0)
           ( sum-done-minutes 0))
      (while (and end (re-search-forward "^-\s+\\[\\([ X]\\)\\]\s+\\(.*?\\)\s*::" end t))
        (let* (( term (match-string 2))
               ( columns (split-string term "[\s/]" 'omit-nulls))
               ( effort-minutes (org-roam-project-duration-to-minutes (nth 5 columns)))
               ( done-minutes (org-roam-project-duration-to-minutes (nth 4 columns)))
               ( todo-minutes (org-roam-project-duration-to-minutes (nth 3 columns))))
          (setq sum-done-minutes (+ sum-done-minutes done-minutes)
                sum-todo-minutes (+ sum-todo-minutes todo-minutes)
                sum-effort-minutes (+ sum-effort-minutes effort-minutes))))
      (org-set-property "day"
                        (concat " " (org-duration-from-minutes sum-done-minutes) " / "
                                (org-duration-from-minutes sum-effort-minutes)
                                " (Done/Effort)  "
                                (org-duration-from-minutes sum-todo-minutes)
                                " (Todo)")))))

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
    (with-current-buffer (org-roam-project-buffer-display)
      (setq buffer-read-only nil)
      (erase-buffer)
      (orp-report-mode)
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
      (org-roam-project-buffer-clean-up (org-roam-project-buffer-today-p time))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))))

(define-derived-mode orp-report-mode org-mode "ORP-Report"
  "Major mode for org-roam-project reports."
  (unless (eq (current-buffer) (get-buffer "*Org Roam Project*"))
    (error "Only one Org-Roam-Project buffer allowed"))
  (setq truncate-lines t)
  (define-key orp-report-mode-map (kbd "g") 'org-roam-project-buffer))
 
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
    (re-search-forward (concat "|\s*" org-roam-project-run-id "\s*|")
                       nil t))
  (beginning-of-line))

(defun org-roam-project-link-at-point ()
  "Return org-roam-project node of link at point."
  (when (and (org-in-regexp org-link-bracket-re)
             (string-match-p "^id:" (match-string-no-properties 1)))
    (org-roam-project-node-from-id (replace-regexp-in-string
                                    "^id:" "" (match-string-no-properties 1)))))

(defun org-roam-project-insert-drawer ( name)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (let* (( limit (save-excursion (or (outline-next-heading) (point))))
           ( found (re-search-forward (concat "^\s*:" name ":") limit t)))
      (unless found
        (unless (re-search-forward "^\s*:END:\n" limit t)
          (beginning-of-line 2))
        (insert ":" (upcase name) ":\n:END:\n")))))

(defun org-roam-project-insert-clock-heading ( &optional title effort)
  "Headings for storing clocking and state information.

TITLE and EFFORT belong to the node at hand.  Returns limit of
current clocking subtree.  TITLE refers to the current title of
the task/node, so when the title changes, a new clocking subtree
is created.  This way it is possible to keep track of title
modifications."
  (unless title
    (setq title (cadar (org-collect-keywords '("title")))))
  (unless effort
    (setq effort (cadar (org-collect-keywords '("effort")))))
  (goto-char (point-min))
  (unless (re-search-forward (org-roam-project-subnodes-re
                                  org-roam-project-clock-heading)
                                 nil t)
    (goto-char (point-max))
    (insert "\n\n* " org-roam-project-clock-heading))
  (unless (re-search-forward (concat "^\\*\\*+\s+" title)
                             (save-excursion (or (outline-get-next-sibling) (point)))
                             t)
    (insert "\n** " title))
  (org-roam-project-set-effort effort)
  (org-roam-project-insert-drawer "LOGBOOK")
  (org-back-to-heading 'invisible-ok)
  (save-excursion (or (outline-get-next-sibling) (point))))

(defun org-roam-project-file-clock-in ()
  (save-excursion
    (let* (( keywords (org-collect-keywords '("title" "effort")))
           ( title (cadr (assoc "TITLE" keywords)))
           ( effort (cadr (assoc "EFFORT" keywords))))
      (org-roam-project-insert-clock-heading title effort)
      (org-clock-in)
      `(,(org-roam-id-at-point) . ,title))))

(defun org-roam-project-node-at-point ()
  (or (org-roam-project-link-at-point)
      (save-excursion
        (beginning-of-line)
        (when (looking-at "-\s+\\[[ X]\\]")
          (org-roam-project-next-link (line-end-position))))
      (org-roam-project-file-p)))

(defun org-roam-project-clock-in ()
  (interactive)
  (let ( node id-title)
    (cond ((setq node (org-roam-project-link-at-point))
           (with-current-buffer (find-file-noselect (org-roam-node-file node))
             (setq id-title (org-roam-project-file-clock-in))))
          ((and (org-roam-project-daily-time-string)
                (org-roam-project-point-in-heading org-roam-project-link-heading)
                (setq node
                      (save-excursion
                        (beginning-of-line)
                        (when (looking-at "-\s+\\[[ X]\\]")
                          (org-roam-project-next-link (line-end-position))))))
           (with-current-buffer (find-file-noselect (org-roam-node-file node))
             (setq id-title (org-roam-project-file-clock-in))))
          ((org-roam-project-file-p)
           (setq id-title (org-roam-project-file-clock-in)))
          ((eq major-mode 'org-mode)
           (org-clock-in))
          (t
           (org-clock-in '(4))))
    (when id-title
      (org-roam-project-daily-add (car id-title) (cdr id-title) nil 'no-dups)
      (message "%s clocked-in." (cdr id-title)))))

(defun org-roam-project-running-clock ()
  (unless (re-search-forward "\\]--\\[" (line-beginning-position) t -1)
    (org-insert-timestamp (current-time) 'with-hm 'inactive "--")
    (org-set-property "STATE" org-roam-project-run-id))
  (end-of-line))

(defun org-roam-project-done-p ( node &optional clocks)
  "Return predicate for done task.

NODE is the project task node.  CLOCKS is the corresponding
clocking property, i.e. a list of org mode clock entries."
  (unless clocks
    (setq clocks (org-roam-project-node-clock node)))
  (let (( in-out-done (org-roam-project-clock-to-time (car clocks))))
    (and (car in-out-done)
         (cadr in-out-done)
         (caddr in-out-done))))

(defun org-roam-project-running-p ( node &optional clocks)
  "Return predicate for running task.

NODE is the project task node.  CLOCKS is the corresponding
clocking property, i.e. a list of org mode clock entries."
  (unless clocks
    (setq clocks (org-roam-project-node-clock node)))
  (let (( in-out-done (org-roam-project-clock-to-time (car clocks))))
    (and (car in-out-done)
         (not (cadr in-out-done))
         (not (caddr in-out-done)))))

(defun org-roam-project-clock-string ( time &optional time-end)
  (cond ((and time time-end)
         (concat (format-time-string (org-time-stamp-format t t) time) "--"
                 (format-time-string (org-time-stamp-format t t) time-end)))
         (time
          (format-time-string (org-time-stamp-format t t) time))
         (t
          nil)))

(defun org-roam-project-clock-to-time ( clock)
  (when clock
    (let (( start 0)
          in out done)
      (when (string-match "\\[.*?\\]" clock start)
        (setq in (org-time-string-to-time (match-string-no-properties 0 clock))
              start (match-end 0))
        (when (string-match "\\[.*?\\]" clock start)
          (setq out (org-time-string-to-time (match-string-no-properties 0 clock)))
          (when (string-match "DONE" clock)
            (setq done t))))
      (list in out done))))

(defun org-roam-project-time-le-p ( A B)
  (or (time-less-p A B) (time-equal-p A B)))

(defun org-roam-project-clock-filter ( clock &optional time-start time-end)
  (let* (( in-out-done (org-roam-project-clock-to-time clock))
         ( in (car in-out-done))
         ( out (cadr in-out-done))
         ( out-c (or out (current-time))))
    (cond ((or (and out time-start (org-roam-project-time-le-p out time-start))
               (and time-end (org-roam-project-time-le-p time-end in)))
           nil)
          ((and (or (not time-start) (org-roam-project-time-le-p time-start in))
                (or (not time-end) (org-roam-project-time-le-p out-c time-end)))
           `(,in . ,out))
          ((and (org-roam-project-time-le-p in time-start)
                (or (not time-end) (org-roam-project-time-le-p out-c time-end)))
           `(,time-start . ,out))
          ((and (or (not time-start) (org-roam-project-time-le-p time-start in))
                (org-roam-project-time-le-p time-end out-c))
           `(,in . ,time-end))
          ((and (org-roam-project-time-le-p in time-start)
                (org-roam-project-time-le-p time-end out-c))
           `(,time-start . ,time-end))
          (t
           nil))))

(defun org-roam-project-done-minutes ( clocks &optional time-end
                                       lookahead-time skip-first-line-done)
  "Return actual working minutes between points in time.

CLOCKS is a list of org time string ranges.  TIME-END is the end
point in time.  When TIME-END is nil, use current time.
LOOKAHEAD-TIME can be used to look ahead TIME-END to determine,
if this task is set to done.  When both TIME-END and
LOOKAHEAD-TIME are nil, return all working minutes since last
done."
  (unless time-end
    (setq time-end (current-time)))
  (let (( sum 0)
        ( first-line-done-p (caddr (org-roam-project-clock-to-time
                                    (car clocks))))
        done)
    (while (and clocks (not done))
      (let* (( clock (pop clocks))
             ( in-out-done (org-roam-project-clock-to-time clock))
             ( in (pop in-out-done))
             ( out (pop in-out-done)))
        (if (and skip-first-line-done first-line-done-p)
            (setq first-line-done-p nil)
          (setq done (when (pop in-out-done) out)))
        (if (and lookahead-time done
                 (time-less-p time-end done) (time-less-p done lookahead-time))
            (setq sum 0)
          (if (time-less-p time-end in)
              (setq done nil)
            (cond ((not out)
                   (setq out (current-time)))
                  ((time-less-p time-end out)
                   (setq out time-end
                         done nil)))
            (unless done
              (setq sum (+ sum (float-time (time-subtract out in)))))))))
    (/ sum 60)))

(defun org-roam-project-remaining-minutes ( effort-minutes done-minutes)
  (if (< effort-minutes done-minutes)
      0
    (- effort-minutes done-minutes)))

(defun org-roam-project-current-duration ( clocks done)
  "From list of clock-lines, get duration since done marker or all time.

CLOCKS is a list of clock-lines, full range or running, in org
time string format.  DONE is the org time string for last logbook
done note.  Return duration in floating point minutes."
  (setq done (and done (org-time-string-to-time done)))
  (let (( start-time (current-time))
        end-time)
    (dolist (clock clocks)
      (let* (( in-out (org-roam-project-clock-to-time clock))
             ( in (car in-out))
             ( out (cdr in-out)))
        (if done
            (when (and (time-less-p in start-time)
                       (time-less-p done in))
              (setq start-time in))
          (when (time-less-p in start-time)
            (setq start-time in)))
        (cond ((and out end-time)
               (when (time-less-p end-time out)
                 (setq end-time out)))
              (out
               (setq end-time out))
              (t
               (setq end-time (current-time))))))
    (/ (float-time (time-subtract (or end-time start-time) start-time)) 60)))

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
        (let (( in-out (org-roam-project-clock-filter clock time)))
          (setq clock (org-roam-project-clock-string (car in-out) (cdr in-out))))
        (when clock
          (insert "\nCLOCK: " clock)
          (org-roam-project-running-clock)
          (org-evaluate-time-range)
          (end-of-line)))
      (insert "\n:END:\n"))))

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
        ( org-roam-dailies-directory "./"))
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

TIME-STRING should have the format %Y-%m-%d, e.g. 2025-05-02.
Query string for the cons cell is %%\"REPEAT\" . \"hallo\"%%,
i.e. the round bracket is replaced by two percent symbols."
  (org-roam-node-from-id
   (caar (org-roam-db-query
          [:select [id]
	               :from nodes
	               :where (like properties $r1)]
          (concat "%%\"CATEGORY\" . \"" time-string "\"%%")))))

;; %%"REPEAT" . "hallo"%%
(defun org-roam-project-repeat-nodes ()
  "Get Org Roam nodes with repeating tasks."
  (mapcar (lambda (raw-id)
            (org-roam-project-node-p (org-roam-node-from-id (car raw-id))))
          (org-roam-db-query
           [:select [id]
	                :from nodes
	                :where (like properties $r1)]
           (concat "%%\"REPEAT\"%%"))))

(defun org-roam-project-daily-node-time ( &optional time)
  "Get Org Roam Dailies node by a Lisp timestamp.

TIME is a Lisp timestamp or nil.  In case of nil, the current time
is used.  If no dailies node exists for the given time, nil is returned."
  (org-roam-project-daily-node
   (format-time-string "%Y-%m-%d" (or time (current-time)))))

(defun org-roam-project-daily-link-exists (id &optional time)
  (let (( buffer-exists (get-buffer (format-time-string "%Y-%m-%d.org" time))))
    (if buffer-exists
        (with-current-buffer buffer-exists
          (member id
                  (org-roam-project-ids-under-heading
                   org-roam-project-link-heading)))
      (member id (org-roam-project-node-subids
                  (org-roam-project-daily-node-time time))))))

(defun org-roam-project-insert-heading ( heading)
  "Make sure heading exists.

HEADING is the title of the heading without cookies.  Leave point
at the end of the corresponding subtree."
  (goto-char (point-min))
  (if (re-search-forward (org-roam-project-subnodes-re heading) nil t)
      (org-end-of-subtree)
    (goto-char (point-max))
    (insert "\n\n* [/][%] " heading)))

(defun org-roam-project-point-in-heading ( heading)
  (save-excursion
    (let (( pos (point))
          ( beg-end (org-roam-project-get-heading heading)))
      (and (<= (car beg-end) pos) (< pos (cdr beg-end))))))

(defun org-roam-project-add-subtask ()
  (interactive)
  (org-roam-project-insert-heading org-roam-project-link-heading)
  (insert "\n- [ ] ")
  (when (org-roam-project-daily-time-string)
    (insert ":: "))
  (let (( completing-read-function 'ivy-completing-read)
        ( templates `(("t" "Task" plain "%?"
                       :target (file+head "%<%Y%m%d%H%M%S>.org"
                                          ,(concat "#+title: ${title}\n"
                                                   "#+filetags: :project:\n"
                                                   "#+effort: %^{effort}"))
                       :empty-lines-before 1
                       :unnarrowed t
                       :jump-to-captured nil
                       :kill-buffer t))))
    (org-roam-node-insert 'org-roam-project-node-effort :templates templates))
  (save-buffer))

(defun org-roam-project-has-time-p ( time-string)
  "Nil if org time string has no day time specified.  Otherwise
return day time string.

TIME-STRING is an org mode time string with or without day time
field.  Returns a day time string in the format
%H:%M (e.g. 12:00)."
  (let (( timestamp (org-timestamp-from-string time-string)))
    (when (org-timestamp-has-time-p timestamp)
      (org-format-timestamp timestamp "%H:%M"))))

(defun org-roam-project-daily-add ( id title &optional time no-dups)
  "Add a link to an org-roam-project task at org-roam-dailies node.

ID and TITLE refers to an org-roam-project task node.  TIME, a
cons cell (lisp-timestamp . day-time-flag), can be used to add
the link at a specific date and with a day-time info.  A non-nil
NO-DUPS makes sure, that no task is added more than one time to
the dailies node.  A non-nil TIME-TERM is used to specify a
plain, scheduled or deadlined day-time."
  (if (and no-dups (org-roam-project-daily-link-exists id (car time)))
      (message "%s already scheduled for today." title)
    (let* (( buffers (org-roam-project-daily-buffer (car time)))
           ( base-buffer (car buffers))
           ( buffer-exists (cdr buffers)))
      (with-current-buffer base-buffer
        (org-roam-project-insert-heading org-roam-project-link-heading)
        (insert "\n- [ ] "
                (if time (concat (cdr time) " ") "")
                ":: " (org-link-make-string (concat "id:" id) title))
        (save-buffer)
        (unless buffer-exists
          (kill-buffer))))))

(defun org-roam-project-goal-add ( task-node goal-node)
  "Add subnode (task) to superior node (goal).

TASK-NODE is the subnode, which is inserted into the GOAL-NODE
subtask list."
  (let* (( file (org-roam-node-file goal-node))
         ( buffer (get-file-buffer file)))
    (with-current-buffer (or buffer
                             (find-file-noselect file))
      (org-roam-project-insert-heading org-roam-project-link-heading)
      (insert "\n- [ ] " (org-link-make-string
                          (concat "id:" (org-roam-node-id task-node))
                          (org-roam-node-title task-node)))
      (save-buffer)
      (unless buffer
        (kill-buffer)))))

(defun org-roam-project-daily-time-string ()
  "Get dailies file time string.

Return file time string from current dailies buffer usually in
the format %Y-%m-%d, or nil if `current-buffer' is not a dailies
buffer."
  (when (and buffer-file-name
             (org-roam-dailies--daily-note-p))
    (file-name-base (buffer-file-name))))

(defun org-roam-project-string-to-time ( &optional time-string)
  (unless time-string
    (setq time-string (format-time-string "%Y-%m-%d")))
  (encode-time (decoded-time-set-defaults (parse-time-string time-string))))

(defun org-roam-project-daily-file-time ()
  (let (( time-string (org-roam-project-daily-time-string)))
    (when time-string
      (org-roam-project-string-to-time time-string))))

(defun org-roam-project-new-node-flags ()
  (when (org-roam-project-file-p)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "#\\+capture-flag:\s+\\([^ ]*\\)\s*\n" nil t)
        (let (( flag (match-string-no-properties 1)))
          (replace-match "")
          (cond ((string= flag "clock-in")
                 (org-roam-project-clock-in))
                ((string= flag "time-prompt")
                 (save-buffer)
                 (let* (( id (org-roam-id-at-point))
                        ( title (cadar (org-collect-keywords '("title"))))
                        ( time (org-roam-project-read-time)))
                   (org-roam-project-daily-add id title time)
                   (message "Added %s to dailies at %s."
                            title (format-time-string "%Y-%m-%d" (car time)))))))))))

(defun org-roam-project-link-daily-add ()
  (interactive)
  (when-let ((node (org-roam-project-link-at-point)))
    (org-roam-project-daily-add
     (org-roam-node-id node)
     (org-roam-node-title node)
     (org-roam-project-read-time))))

(add-hook 'org-roam-capture-new-node-hook 'org-roam-project-new-node-flags)

(org-link-set-parameters
 "dailies"
 :follow 'org-roam-project-daily-follow-link
 :face 'org-link)

(defun org-roam-project-daily-follow-link ( path arg)
  (ignore arg)
  (let (( time (org-roam-project-string-to-time path))
        ( org-roam-directory (expand-file-name org-roam-dailies-directory
                                               org-roam-directory))
        ( org-roam-dailies-directory "./"))
    (org-roam-capture- :goto '(4)
                       :keys "d"
                       :node (org-roam-node-create)
                       :templates org-roam-dailies-capture-templates
                       :props (list :override-default-time time))
    (run-hooks 'org-roam-dailies-find-file-hook)))

(defun org-roam-project-get-time-frame ( term effort-minutes)
  (let (( start-minutes (org-duration-to-minutes
                         (or (match-string-no-properties 1 term) -1.0)))
        ( separator (match-string-no-properties 2 term))
        ( stop-minutes (org-duration-to-minutes
                        (or (match-string-no-properties 3 term) -1.0))))
    (cond ((and (string= separator ">") (< -1 start-minutes))
           (concat (org-duration-from-minutes start-minutes)
                   ">"
                   (org-duration-from-minutes
                    (+ start-minutes effort-minutes))))
          ((and (string= separator "<") (< -1 stop-minutes))
           (concat (org-duration-from-minutes
                    (- stop-minutes effort-minutes))
                   "<"
                   (org-duration-from-minutes stop-minutes))))))

(defun org-roam-property-center-string ( string space)
  "Center string.

Center STRING in available SPACE."
  (let* (( prepost (/ (- space (length string)) 2))
         ( prepostfix (make-string (if (< prepost 0) 0 prepost) ? )))
    (format (concat "%-" (number-to-string space) "s")
            (concat prepostfix string prepostfix ))))

(defun org-roam-project-recent-done-time ( dones &optional end-time)
  (unless end-time (setq end-time (current-time)))
  (let ( found time)
    (while (and dones (not found))
      (when (time-less-p (setq time (org-time-string-to-time (pop dones)))
                         end-time)
        (setq found time)))
    found))

(defun org-roam-project-time-column (term effort-minutes)
  (cond ((and (not term) effort-minutes )
         (org-roam-property-center-string
          (propertize (concat "(" (org-duration-from-minutes effort-minutes) ")")
                      'font-lock-face 'dired-ignored)
          11))
        ((and effort-minutes
              (string-match (concat "\\([0-2]?[0-9]:[0-5][0-9]\\)?"
                                    "\\([<>]\\)"
                                    "\\([0-2]?[0-9]:[0-5][0-9]\\)?")
                            term))
         (org-roam-project-get-time-frame term effort-minutes))
        ((string-match "^\\([0-2]?[0-9]:[0-5][0-9]\\)" term)
         (match-string-no-properties 1 term))
        (effort-minutes
         (org-roam-property-center-string
          (propertize (concat "(" (org-duration-from-minutes effort-minutes) ")")
                  'font-lock-face 'dired-ignored)
          11))
        (t
         "")))

(defun org-roam-project-format-line ( day-start node)
  (beginning-of-line)
  (when (looking-at "-\s+\\[\s\\]\s\\(.*?\\)::")
    (let* (( term (match-string-no-properties 1))
           ( term-beg (match-beginning 1))
           ( term-end (match-end 1))
           ( effort (org-roam-project-node-effort node))
           ( effort-minutes (org-duration-to-minutes effort))
           ( clocks (org-roam-project-node-clock node))
           ( done-minutes (org-roam-project-done-minutes clocks))
           ( done-previous-minutes (org-roam-project-done-minutes
                                    clocks day-start
                                    (time-add day-start (days-to-time 1))))
           ( done-today-minutes (- done-minutes done-previous-minutes))
           ( todo-minutes (- effort-minutes done-minutes))
           ( todo-previous-minutes (org-roam-project-remaining-minutes
                                    effort-minutes done-previous-minutes)))
      (delete-region term-beg term-end)
      (goto-char term-beg)
      (insert (format "%-11s %s/%s %s %s/%s %s "
                      (org-roam-project-time-column (car (split-string term))
                                                    effort-minutes)
                      (propertize (org-duration-from-minutes done-minutes)
                                  'help-echo "Duration since last done")
                      (propertize effort
                                  'help-echo "Effort")
                      (propertize (org-roam-project-sign-prefix
                                   (org-duration-from-minutes todo-minutes))
                                  'help-echo "Remaining time to reach effort")
                      (propertize (org-duration-from-minutes done-today-minutes)
                                  'help-echo "Duration for today")
                      (propertize (org-duration-from-minutes todo-previous-minutes)
                                  'help-echo "Fixed Effort for today")
                      (propertize (org-duration-from-minutes done-previous-minutes)
                                  'help-echo "Done before today")
                      )))))

(defun org-roam-project-format-all ()
  "Format all task entries of dailies tasks list.

Consider only open tasks.  If a task occurs more than one time in
the list, format only the first in list but ignore all following
entries of this task.  This way, one can schedule unlimited
number of the same task per day."
  (save-excursion
    (let (( day-start (org-roam-project-daily-time))
          ( beg-end (org-roam-project-subtree org-roam-project-link-heading))
          ids)
      (goto-char (cdr beg-end))
      (while (re-search-forward "^-\s+\\[\s\\]\s+.*?::" (car beg-end) t -1)
        (let* (( pos (match-beginning 0))
               ( node (org-roam-project-next-link (line-end-position)))
               ( id (and node (org-roam-node-id node))))
          (unless (member id ids)
            (setq ids (cons id ids))
            (org-roam-project-format-line day-start node))
          (goto-char pos))))))

(defun org-roam-project-spread-goal ()
  (let (( beg-end (org-roam-project-subtree org-roam-project-link-heading))
        node)
    (goto-char (cdr beg-end))
    (while (setq node (org-roam-project-next-link (car beg-end) 'backward))
      (when (org-roam-project-subnodes node)
        (delete-region (line-beginning-position) (line-end-position))
        (let* (( pos (line-beginning-position))
               ( goal-tasks (org-roam-project-goals-tasks (list node)))
               ( tasks (org-roam-project-goal-tasks (car goal-tasks))))
          (dolist ( task (nreverse tasks))
            (insert "- [ ] :: " (org-link-make-string
                                 (concat "id:" (org-roam-node-id task))
                                 (org-roam-node-title task))
                    "\n"))
          (delete-char -1)
          (goto-char pos))))
    (goto-char (car beg-end))))

(defun org-roam-project-date-item ()
  "Insert description list item with current date-time term."
  (interactive)
  (beginning-of-line)
  (insert "- "
          (format-time-string (org-time-stamp-format 'with-time 'inactive)
                              (current-time))
          " :: "))

(defun org-roam-project-log-done ( node &optional undone)
  (with-current-buffer (find-file-noselect (org-roam-node-file node))
    (let* (( keywords (org-collect-keywords '("title" "effort")))
           ( title (cadr (assoc "TITLE" keywords)))
           ( effort (cadr (assoc "EFFORT" keywords)))
           ( limit (org-roam-project-insert-clock-heading title effort)))
      (when (re-search-forward "^\s*:LOGBOOK:\s*\n" limit t)
        (if undone
            (when (looking-at "^\s*DONE")
              (delete-region (line-beginning-position) (line-beginning-position 2))
              (save-buffer))
          (unless (looking-at "^\s*CLOCK")
            (user-error "No clock found (only clocked tasks can be set done)"))
          (insert "DONE\n")
          (if (looking-at "^\s*CLOCK:\s*\\[[^]]*\\][^-]")
              (org-clock-out)
            (save-buffer)))))))

(defun org-roam-project-valid-checkbox ()
  (beginning-of-line)
  (save-excursion
    (let* (( time (org-roam-project-daily-time))
           ( pos (line-beginning-position))
           ( node (org-roam-project-next-link (line-end-position) nil 'update-db))
           ( id (org-roam-node-id node))
           ( first-unchecked (concat "^-\s+\\[\s\\]\s.*?::\s+.*?" id))
           ( last-checked (concat "^-\s+\\[X\\]\s.*?::\s+.*?" id))
           ( clocks (org-roam-project-node-clock node))
           ( last-clock (org-roam-project-clock-to-time (car clocks)))
           ( in (car last-clock))
           ( done (caddr last-clock))
           ( beg-end (org-roam-project-get-heading org-roam-project-link-heading)))
      (cond (done
             (goto-char (cdr beg-end))
             (and (re-search-forward last-checked (car beg-end) t -1)
                  (= (line-beginning-position) pos)
                  `(,node . uncheck)))
            ((and (not done) (time-less-p time in))
             (goto-char (car beg-end))
             (and (re-search-forward first-unchecked (cdr beg-end) t)
                  (= (line-beginning-position) pos)
                  `(,node . check)))))))

(defun org-roam-project-toggle-checkbox (&optional toggle-presence)
  "Toggle checkbox for org-roam-project daily task list.

TOGGLE-PRESENCE is passed to org native checkbox toggle function.
When not in daily task list, use native org function
`org-toggle-checkbox' instead."
  (interactive "P")
  (org-roam-project-format-all)
  (if (and (org-roam-project-daily-time-string)
           (org-roam-project-point-in-heading org-roam-project-link-heading))
      (let* (( node-op (org-roam-project-valid-checkbox))
             ( node (car node-op))
             ( op (cdr node-op)))
        (cond ((and (eq op 'check)
                    (looking-at "-\s+\\[\\(\s\\)\\].*?::"))
               (replace-match "X" t nil nil 1)
               (org-roam-project-log-done node))
              ((and (eq op 'uncheck)
                    (looking-at "-\s+\\[\\(X\\)\\].*?::"))
               (replace-match " " t nil nil 1)
               (org-roam-project-log-done node 'undone)))
        (org-update-checkbox-count))
    (org-toggle-checkbox toggle-presence)))

(defun org-roam-project-goal-fontify ( limit)
  (when (or (org-roam-project-file-p)
            (org-roam-project-daily-file-time))
    (let (( beg-end (save-excursion
                      (org-roam-project-subtree
                       org-roam-project-link-heading))))
      (when (and beg-end
                 (re-search-forward "^-\s+\\(\\[\s\\]\\)\s" limit t)
                 (< (car beg-end) (point)) (< (point) (cdr beg-end)))
        (let* (( beg (match-beginning 1))
               ( end (match-end 1))
               ( node (save-excursion
                        (org-roam-project-next-link (line-end-position))))
               ( running (org-roam-project-running-p node)))
          (when running
            (add-face-text-property beg end '(:background "cyan"))
            t))))))

(defun org-roam-project-daily-fontify ( limit)
  (when (org-roam-project-daily-file-time)
    (cond ((re-search-forward "\\([+-]\\)[0-9]?[0-9]:[0-5][0-9]" limit t)
           (let (( color (if (string= "+" (match-string-no-properties 1))
                             "green" "orange")))
             (add-face-text-property (match-beginning 0) (match-end 0)
                                     `(:foreground ,color))
             t))
          ((re-search-forward (concat "\\(\s[0-9]?[0-9]:[0-5][0-9]\s\\)"
                                      "/"
                                      "\\(\s[0-9]?[0-9]:[0-5][0-9]\s\\)"
                                      "\s*(Done/Effort)")
                              limit t)
           (add-face-text-property (match-beginning 1) (match-end 1)
                                     `(:weight bold :background "yellow"))
           (add-text-properties (match-beginning 2) (match-end 2)
                                `(face tty-menu-enabled-face))
           t)
          ((re-search-forward (concat "\\(\s[0-9]?[0-9]:[0-5][0-9]\s\\)"
                                      "\s*(Todo)")
                              limit t)
           (add-face-text-property (match-beginning 1) (match-end 1)
                                   `(:weight bold
                                             :background "yellow" :foreground "purple"))
           t)
          (t
           nil))))

(defun org-roam-project-font-lock-add-keywords ()
  (add-to-list 'org-font-lock-extra-keywords
               '( org-roam-project-goal-fontify ( 0 nil append t))
               'append)
  (add-to-list 'org-font-lock-extra-keywords
               '( org-roam-project-daily-fontify ( 0 nil append t))
               'append))

(add-hook 'org-font-lock-set-keywords-hook
          #'org-roam-project-font-lock-add-keywords)

(defun org-roam-project-ivy-action ( item)
  (let (( task-node (org-roam-project-node-at-point))
        ( goal-node (org-roam-node-from-id (cdr item))))
    (org-roam-project-goal-add task-node goal-node)
    (message "\"%s\" added to \"%s\"."
             (org-roam-node-title task-node)
             (org-roam-node-title goal-node))))

(defun org-roam-project-ivy-exit-action ()
  (interactive)
  (ivy-exit-with-action 'org-roam-project-ivy-action))

(defun org-roam-project-ivy-add-to-goal ()
  (interactive)
  (let* (( root-nodes (org-roam-project-root-nodes (org-roam-project-nodes)))
         ( goals-tasks (org-roam-project-goals-tasks root-nodes))
         ( goals (car goals-tasks))
         collection)
    (dolist (project goals)
      (dolist ( node-level project)
        (let* (( node (car node-level)))
          (when (org-roam-project-subnodes node)
            (setq collection
                  (cons `(,(concat (make-string (* (1- (cdr node-level)) 2) ? )
                                   (org-roam-node-title node))
                          . ,(org-roam-node-id node))
                        collection))))))
    (ivy-read "Insert into Goal? " (nreverse collection)
              :keymap (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "<return>")
                                    'org-roam-project-ivy-exit-action)
                        (define-key map (kbd "<mouse-1>")
                                    'org-roam-project-ivy-exit-action)
                        map))))

(defun org-roam-project-update-subnodes ()
  (let* (( beg-end (org-roam-project-get-heading org-roam-project-link-heading))
         ( beg (car beg-end))
         ( end (cdr beg-end))
         ( effort-sum 0)
         ( done-sum 0)
         ( done-state-sum t))
    (unless end (user-error "No Org Roam Project Heading found"))
    (save-match-data
      (goto-char end)
      (while (re-search-forward "^-\s+\\[\\([ X]\\)\\]\\(\s\\)\\(\s*.*?::\s+\\)?"
                                beg t -1)
        (let (( effort-p (match-string 3))
              ( effort-cumulated 0)
              ( done-cumulated 0)
              ( done-state-cumulated t))
          (save-match-data
            (let* (( root-node (org-roam-project-next-link (line-end-position)))
                   ( tree (car (org-roam-project-preorder (list root-node)))))
              (dolist ( node-level tree)
                (unless (org-roam-project-subnodes (car node-level))
                  (let* (( node (car node-level))
                         ( effort-minutes (org-duration-to-minutes
                                   (org-roam-project-node-effort node)))
                         ( clocks (org-roam-project-node-clock node))
                         ( done-minutes (org-roam-project-done-minutes
                                         clocks nil nil 'skip-first-line-done))
                         ( done-state (org-roam-project-done-p node clocks)))
                    (setq effort-cumulated (+ effort-minutes effort-cumulated)
                          done-cumulated (+ done-minutes done-cumulated))
                    (unless done-state (setq done-state-cumulated nil)))))))
          (replace-match (if done-state-cumulated "X" " ") t nil nil 1)
          (replace-match (concat (unless effort-p " ")
                                 (org-duration-from-minutes done-cumulated)
                                 "/" (org-duration-from-minutes effort-cumulated)
                                 " :: ")
                           t nil nil (if effort-p 3 2))
          (setq effort-sum (+ effort-cumulated effort-sum)
                done-sum (+ done-cumulated done-sum))
          (unless done-state-cumulated (setq done-state-sum nil)))
        (beginning-of-line)))
    (when (match-string 2)
      (replace-match (if done-state-sum "DONE" "TODO") t nil nil 2))
    (org-update-checkbox-count)
    (org-roam-set-keyword "effort" (org-duration-from-minutes effort-sum))))

(defun org-roam-project-duration-from-times ( in-time out-time)
  (let* (( diff-minutes (/ (float-time (time-subtract out-time in-time)) 60)))
    (org-duration-from-minutes diff-minutes)))

(defun org-roam-project-time-range-line ( in-time out-time &optional node)
  (let (( id (when node (org-roam-node-id node)))
        ( in-hour (format-time-string "%H:%M" in-time))
        ( in-date (format-time-string "%Y-%m-%d" in-time))
        ( in-string (format-time-string "%Y-%m-%d %H:%M" in-time))
        ( out-hour (format-time-string "%H:%M" out-time))
        ( out-date (format-time-string "%Y-%m-%d" out-time))
        ( out-string (format-time-string "%Y-%m-%d %H:%M" out-time)))
    (insert (format "%5s" (propertize in-hour 'in in-string 'id id))
            " ⇅ " (format "%5s" (propertize out-hour 'out out-string))
            " → " (format "%5s" (org-roam-project-duration-from-times
                                 in-time out-time))
            (format "%30s" (propertize (concat in-date " → " out-date)
                                       'face 'dired-ignored))
            "\n")))

(defun org-roam-project-time-range-buffer ( &optional time-start time-end closed node)
  (unless time-start (setq time-start (org-roam-project-daily-time)))
  (unless time-end (setq time-end (time-add time-start (days-to-time 1))))
  (when (time-less-p (current-time) time-end)
    (setq time-end (current-time)))
  (let (( slots (if closed
                    (org-roam-project-time-slots time-start time-end)
                  (org-roam-project-time-slots-open time-start time-end))))
    (switch-to-buffer-other-window "*Org Roam Project Edit*")
    (erase-buffer)
    (orp-edit-mode)
    (insert "Free Slots in Range:\n"
            (format-time-string "%d.%m.%Y %H:%M" time-start) " → "
            (format-time-string "%d.%m.%Y %H:%M" time-end)
            "\n\n")
    (when node
      (insert (org-roam-node-title node) "\n\n"))
    (dolist ( slot slots)
      (let (( in-time (car slot))
            ( out-time (cdr slot)))
        (org-roam-project-time-range-line in-time out-time node))))
  (set-buffer-modified-p nil))

(defvar org-roam-project-time-delta 5)
(defvar org-roam-project-time-range-re
  (concat "\s?\\([0-2]?[0-9]:[0-5][0-9]\\)"
          "\s⇅\s"
          "\s?\\([0-2]?[0-9]:[0-5][0-9]\\)"
          "\s→\s"
          "\s?\\([0-9]?[0-9]:[0-5][0-9]\\)"
          "\s+"
          "\\([0-9-]+\\)"
          "\s→\s"
          "\\([0-9-]+\\)"))
  
(defun org-roam-project-time-range-buffer-return ()
  (interactive)
  (when (eq major-mode 'orp-edit-mode)
    (beginning-of-line)
    (if (looking-at org-roam-project-time-range-re)
        (let* (( pos (match-beginning 1))
               ( in-time (org-roam-project-string-to-time
                          (concat (match-string-no-properties 4) " "
                                  (match-string-no-properties 1))))
               ( out-time (org-roam-project-string-to-time
                           (concat (match-string-no-properties 5) " "
                                   (match-string-no-properties 2))))
               ( node (org-roam-node-from-id (get-text-property pos 'id))))
          (when node
            (org-roam-project-node-clock-add node in-time out-time))
          (kill-this-buffer))
      (kill-this-buffer))))

(define-derived-mode orp-edit-mode fundamental-mode "ORP-Edit"
  "Major mode for org-roam-project edit buffer."
  (unless (eq (current-buffer) (get-buffer "*Org Roam Project Edit*"))
    (error "Only one Org-Roam-Project buffer allowed"))
  (setq truncate-lines t)
  (define-key orp-edit-mode-map (kbd "q") 'kill-this-buffer)
  (define-key orp-edit-mode-map (kbd "<return>") 'org-roam-project-time-range-buffer-return)
  (define-key orp-edit-mode-map (kbd "S-<up>") 'org-roam-project-time-up)
  (define-key orp-edit-mode-map (kbd "S-<down>") 'org-roam-project-time-down))

(defun org-roam-project-time-up ( &optional down)
  (interactive)
  (let* (( pos (point))
         ( in-string (get-text-property pos 'in))
         ( in-time (when in-string (org-roam-project-string-to-time in-string)))
         ( out-string (get-text-property pos 'out))
         ( out-time (when out-string (org-roam-project-string-to-time out-string))))
    (beginning-of-line)
    (when (looking-at org-roam-project-time-range-re)
      (let* (( id (get-text-property (match-beginning 1) 'id))
             ( in-time-set (org-roam-project-string-to-time
                            (concat (match-string-no-properties 4) " "
                                    (match-string-no-properties 1))))
             ( out-time-set (org-roam-project-string-to-time
                             (concat (match-string-no-properties 5) " "
                                     (match-string-no-properties 2))))
             ( new-time (if down
                            (time-subtract (if in-time in-time-set out-time-set)
                                           (* org-roam-project-time-delta 60))
                          (time-add (if in-time in-time-set out-time-set)
                                    (* org-roam-project-time-delta 60)))))
        (if in-time
            (when (and (org-roam-project-time-le-p in-time new-time)
                       (org-roam-project-time-le-p new-time out-time-set))
              (replace-match (propertize (format-time-string "%H:%M" new-time)
                                         'in in-string 'id id)
                             t nil nil 1)
              (replace-match (propertize (format-time-string "%Y-%m-%d" new-time)
                                         'face 'dired-ignored)
                             t nil nil 4)
              (replace-match (org-duration-from-minutes
                              (/ (float-time (time-subtract out-time-set new-time)) 60))
                             t nil nil 3))
          (when (and (org-roam-project-time-le-p in-time-set new-time)
                     (org-roam-project-time-le-p new-time out-time))
            (replace-match (propertize (format-time-string "%H:%M" new-time)
                                       'out out-string)
                           t nil nil 2)
            (replace-match (propertize (format-time-string "%Y-%m-%d" new-time)
                                       'face 'dired-ignored)
                           t nil nil 5)
            (replace-match (org-duration-from-minutes
                            (/ (float-time (time-subtract new-time in-time-set)) 60))
                           t nil nil 3)))))
    (goto-char pos))
  (set-buffer-modified-p nil))

(defun org-roam-project-time-down ()
  (interactive)
  (org-roam-project-time-up 'down))

(defun org-roam-project-time-slots ( time-start time-end)
  (save-excursion
    (let (( end (cdr (org-roam-project-subtree org-roam-project-link-heading)))
          time-slots)
      (while (re-search-forward "^-\s+\\[[\sX]\\]\s+.*?::" end t)
        (let* (( node (org-roam-project-next-link (line-end-position)))
               ( clocks (org-roam-project-node-clock node))
               ( in-out-done (org-roam-project-clock-to-time (car clocks)))
               ( in (car in-out-done))
               ( out (or (cadr in-out-done) (current-time))))
          (while (and clocks (time-less-p time-start out))
            (let* (( in-out (org-roam-project-clock-filter (pop clocks) time-start time-end)))
              (when (car in-out)
              (setq in (car in-out)
                    out (or (cdr in-out) (current-time))
                    time-slots (cons `(,in . ,out) time-slots)))))))
      (setq time-slots (sort time-slots (lambda ( a b)
                                          (time-less-p (car a) (car b)))))
        time-slots)))
              
(defun org-roam-project-time-slots-open ( time-start time-end)
  (let* (( time-slots (org-roam-project-time-slots time-start time-end))
         ( in time-start)
         open-slots)
    (dolist ( slot time-slots)
      (setq open-slots (cons `(,in . ,(car slot)) open-slots)
            in (cdr slot)))
    (setq open-slots (cons `(,in . ,time-end) open-slots))
    (nreverse open-slots)))

(defun org-roam-project-clock-add ()
  (interactive)
  (beginning-of-line)
  (when (and (org-roam-project-daily-time)
             (org-roam-project-point-in-heading org-roam-project-link-heading))
    (let* (( node (org-roam-project-next-link (line-end-position) nil 'update-db)))
      (org-roam-project-time-range-buffer nil nil nil node))))

(defun org-roam-project-node-clock-add ( node new-in new-out)
  (when (org-roam-project-node-p node)
    (with-current-buffer (find-file-noselect (org-roam-node-file node))
      (goto-char (point-min))
      (let (( limit (org-roam-project-insert-clock-heading))
              found logbook-end)
        (while (and (not found)
                    (re-search-forward org-logbook-drawer-re limit t))
            (goto-char (match-beginning 0))
            (setq logbook-end (match-end 0))
            (end-of-line) ;; point at end of begin logbook
            (while (and (not found)
                        (re-search-forward
                         "^\s*CLOCK:\s*\\(\\[.*?\\]\\)\\(--\\(\\[.*?\\]\\)\\)?"
                         logbook-end t))
              (when (time-less-p (org-time-string-to-time
                                  (match-string-no-properties 1))
                                 new-in)
                (setq found t))))
        (cond (found ;; point after clock-range
               (re-search-backward "DONE" (line-beginning-position 0) t)
               (beginning-of-line))
              (t ;; point after clock-range or point at end of begin logbook
               (beginning-of-line 2))))
      (insert "CLOCK: "
              (format-time-string (org-time-stamp-format t t) new-in) "--"
              (format-time-string (org-time-stamp-format t t) new-out) "\n")
      (end-of-line 0)
      (org-evaluate-time-range)
      (save-buffer))))

(cl-defmethod org-roam-node-orp ((node org-roam-node))
  "Return value of state keyword for current node.

NODE is reference to the current node."
  (let* (( properties (org-roam-node-properties node))
         ( effort (cdr (assoc "EFFORT" properties)))
         ( subnodes (org-roam-project-subnodes nil properties)))
    (if effort
        (concat " ~"
                (if subnodes
                    (concat "g" (number-to-string (length subnodes)))
                  (concat "t" effort)))
      "")))

(defun org-roam-project-links-title-update ()
  (save-excursion
    (let (( end (cdr (org-roam-project-subtree org-roam-project-link-heading)))
          node)
      (while (setq node (org-roam-project-next-link end))
        (let (( title (org-roam-node-title node))
              ( link (match-string-no-properties 1))
              ( description (match-string-no-properties 2)))
          (when (and (string= link
                              (concat "id:" (org-roam-node-id node)))
                     (not (string= description title)))
            (replace-match title t nil nil 2)))))))

(provide 'org-roam-project)

;;; org-roam-project.el ends here
