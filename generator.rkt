#lang racket

;; Import required libraries
(require markdown)
(require racket/file)
(require racket/string)
(require web-server/templates)
(require racket/list)

;; Define a struct for post metadata
(struct post (title date content output-file) #:mutable #:transparent)

;; Function to ensure output directory exists
(define (ensure-output-dir)
  (define output-dir "output")
  (when (not (directory-exists? output-dir))
    (make-directory output-dir)))

;; Function to get list of markdown files from posts directory
(define (get-markdown-files)
  (define posts-dir "posts")
  (if (directory-exists? posts-dir)
      (filter (λ (f) (string-suffix? f ".md"))
              (map path->string (directory-list posts-dir)))
      (error "Posts directory not found")))

;; Function to parse frontmatter
(define (parse-frontmatter content)
  (define lines (string-split content "\n"))
  (if (and (> (length lines) 2)
           (string=? (first lines) "---"))
      (let* ([end-idx (index-of (cdr lines) "---")]
             [frontmatter-lines (take (cdr lines) end-idx)]
             [content-lines (drop (cdr lines) (+ end-idx 1))]
             [post-data (post "Untitled" "No date" "" "")])
        (for ([line frontmatter-lines])
          (when (regexp-match #px"^([^:]+):\\s*\"?([^\"]+)\"?$" line)
            (let ([matches (regexp-match #px"^([^:]+):\\s*\"?([^\"]+)\"?$" line)])
              (define key (string-trim (second matches)))
              (define value (string-trim (third matches)))
              (cond
                [(string=? key "title") (set-post-title! post-data value)]
                [(string=? key "date") (set-post-date! post-data value)]
                [else (void)]))))
        (values post-data
                (string-join content-lines "\n")))
      (values (post "Untitled" "No date" "" "") content)))

;; Function to process a single markdown file
(define (process-markdown-file filename)
  (define input-path (build-path "posts" filename))
  (define output-filename 
    (path->string (path-replace-extension filename #".html")))
  (define output-path (build-path "output" output-filename))
  
  (define content (file->string input-path))
  (define-values (post-data markdown-content) (parse-frontmatter content))
  
  (define parsed-content (parse-markdown markdown-content))
  (define html-content 
    (string-join
     (map xexpr->string parsed-content)
     "\n"))

  ;; Set the content field of the post struct
  (set-post-content! post-data html-content)
  (set-post-output-file! post-data output-filename)
  
  ;; Render template with bindings
  (define full-html
    (let ([title (post-title post-data)]
          [date (post-date post-data)]
          [content html-content])
      (include-template "templates/base.html")))
  
  (with-output-to-file output-path
    (λ () (display full-html))
    #:exists 'replace)
  
  ;; Return the post data for later use
  post-data)

;; Function to generate post list HTML
(define (generate-post-list-html posts)
  (string-join
   (for/list ([post (sort posts (λ (a b) (string>? (post-date a) (post-date b))))])
     (format "<li><a href=\"~a\">~a</a><span class=\"post-date\">~a</span></li>"
             (post-output-file post)
             (post-title post)
             (post-date post)))
   "\n"))

;; Function to generate index page
(define (generate-index-html markdown-files)
  (define posts
    (for/list ([file markdown-files])
      (process-markdown-file file)))
  
  (define posts-html (generate-post-list-html posts))
  
  (define full-html
    (let ([posts-html posts-html])
      (include-template "templates/index.html")))
  
  (with-output-to-file (build-path "output" "index.html")
    (λ () (display full-html))
    #:exists 'replace))

;; Main function
(define (main)
  (ensure-output-dir)
  (define markdown-files (get-markdown-files))
  (for ([file markdown-files])
    (printf "Processing ~a...~n" file))
  (printf "Generating index.html...~n")
  (generate-index-html markdown-files)
  (printf "Done!~n"))

;; Run the main function
(main)
