#lang racket

;; Import required libraries
(require markdown)
(require racket/file)
(require racket/string)
(require web-server/templates)
(require racket/list)

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
             [frontmatter (make-hash)])
        (for ([line frontmatter-lines])
          (when (regexp-match #px"^([^:]+):\\s*\"?([^\"]+)\"?$" line)
            (let ([matches (regexp-match #px"^([^:]+):\\s*\"?([^\"]+)\"?$" line)])
              (hash-set! frontmatter 
                        (string-trim (second matches))
                        (string-trim (third matches))))))
        (values frontmatter
                (string-join content-lines "\n")))
      (values (make-hash) content)))

;; Function to process a single markdown file
(define (process-markdown-file filename)
  (define input-path (build-path "posts" filename))
  (define output-filename 
    (path->string (path-replace-extension filename #".html")))
  (define output-path (build-path "output" output-filename))
  
  (define content (file->string input-path))
  (define-values (frontmatter markdown-content) (parse-frontmatter content))
  
  (define title (hash-ref frontmatter "title" "Untitled"))
  (define date (hash-ref frontmatter "date" "No date"))
  
  (define parsed-content (parse-markdown markdown-content))
  (define html-content 
    (string-join
     (map xexpr->string parsed-content)
     "\n"))

  ;; Render template with bindings
  (define full-html
    (let ([title title]
          [date date]
          [content html-content])
      (include-template "templates/base.html")))
  
  (with-output-to-file output-path
    (λ () (display full-html))
    #:exists 'replace))

;; Function to generate post list HTML
(define (generate-post-list-html posts)
  (string-join
   (for/list ([post (sort posts (λ (a b) (string>? (hash-ref a "date" "") (hash-ref b "date" ""))))])
     (format "<li><a href=\"~a\">~a</a><span class=\"post-date\">~a</span></li>"
             (hash-ref post "output-file")
             (hash-ref post "title")
             (hash-ref post "date")))
   "\n"))

;; Function to generate index page
(define (generate-index-html markdown-files)
  (define posts
    (for/list ([file markdown-files])
      (define content (file->string (build-path "posts" file)))
      (define-values (frontmatter _) (parse-frontmatter content))
      (hash-set! frontmatter "output-file" 
                 (path->string (path-replace-extension file #".html")))
      frontmatter))
  
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
    (printf "Processing ~a...~n" file)
    (process-markdown-file file))
  (printf "Generating index.html...~n")
  (generate-index-html markdown-files)
  (printf "Done!~n"))

;; Run the main function
(main)
