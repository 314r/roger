#lang racket

;; Import required libraries
(require markdown)
(require racket/file)
(require racket/string)
(require web-server/templates)
(require racket/list)

;; Define a struct for post metadata
(struct post (title date content markdown-content output-file backlinks) #:mutable #:transparent)

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
             [post-data (post "Untitled" "No date" "" "" "" '())])
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
      (values (post "Untitled" "No date" "" "" "" '()) content)))

;; Function to extract links from markdown content
(define (extract-links markdown-content)
  (define link-pattern #px"\\[([^\\]]+)\\]\\(([^\\)]+)\\)")
  (define links '())
  
  (let loop ([remaining markdown-content])
    (define matches (regexp-match link-pattern remaining))
    (when matches
      (define link-url (third matches))  ;; The URL is the third match
      (set! links (cons link-url links))
      (define match-length (string-length (first matches)))
      (define remaining-text (substring remaining match-length))
      (loop remaining-text)))
  
  links)

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

  ;; Set the content fields of the post struct
  (set-post-content! post-data html-content)
  (set-post-markdown-content! post-data markdown-content)
  (set-post-output-file! post-data output-filename)
  
  ;; Return the post data for later use
  post-data)

;; Function to build backlinks between posts
(define (build-backlinks posts)
  (define filename->post (make-hash))
  
  ;; First, create a mapping from output filenames to posts
  (for ([post posts])
    (hash-set! filename->post (post-output-file post) post))
  
  ;; Then, for each post, extract links and update backlinks
  (for ([post posts])
    (define current-file (post-output-file post))
    (define links (extract-links (post-markdown-content post)))
    (printf "Links in ~a: ~a~n" current-file links)
    (for ([link links])
      ;; Skip self-referential links
      (when (and (hash-has-key? filename->post link)
                 (not (string=? link current-file)))
        (define target-post (hash-ref filename->post link))
        (define current-backlinks (post-backlinks target-post))
        (set-post-backlinks! target-post 
                            (if (member current-file current-backlinks)
                                current-backlinks
                                (cons current-file current-backlinks))))))
  
  ;; Print backlinks for debugging
  (for ([post posts])
    (printf "Backlinks for ~a: ~a~n" (post-output-file post) (post-backlinks post)))
  
  ;; Return the updated posts
  posts)

;; Function to generate backlinks HTML
(define (generate-backlinks-html post filename->post)
  (define backlinks (post-backlinks post))
  (if (null? backlinks)
      ""
      (string-append
       "<div class=\"backlinks\">\n"
       "<h3>Backlinks</h3>\n"
       "<ul>\n"
       (string-join
        (for/list ([backlink backlinks])
          (define linking-post (hash-ref filename->post backlink #f))
          (if linking-post
              (format "<li><a href=\"~a\">~a</a></li>"
                      backlink
                      (post-title linking-post))
              ""))
        "\n")
       "\n</ul>\n"
       "</div>")))

;; Function to generate post list HTML
(define (generate-post-list-html posts)
  (string-join
   (for/list ([post (sort posts (λ (a b) (string>? (post-date a) (post-date b))))])
     (format "<li><a href=\"~a\">~a</a><span class=\"post-date\">~a</span></li>"
             (post-output-file post)
             (post-title post)
             (post-date post)))
   "\n"))

;; Function to render all posts with backlinks
(define (render-posts posts)
  (define filename->post (make-hash))
  
  ;; Create a mapping from output filenames to posts
  (for ([post posts])
    (hash-set! filename->post (post-output-file post) post))
  
  ;; Render each post
  (for ([post posts])
    (define output-path (build-path "output" (post-output-file post)))
    (define backlinks-html (generate-backlinks-html post filename->post))
    
    (printf "Rendering ~a with backlinks: ~a~n" (post-output-file post) (post-backlinks post))
    
    ;; Render template with bindings
    (define full-html
      (let ([title (post-title post)]
            [date (post-date post)]
            [content (string-append (post-content post) "\n" backlinks-html)])
        (include-template "templates/base.html")))
    
    (with-output-to-file output-path
      (λ () (display full-html))
      #:exists 'replace)))

;; Function to generate index page
(define (generate-index-html markdown-files)
  (define posts
    (for/list ([file markdown-files])
      (process-markdown-file file)))
  
  ;; Build backlinks between posts
  (define posts-with-backlinks (build-backlinks posts))
  
  ;; Render all posts with backlinks
  (render-posts posts-with-backlinks)
  
  (define posts-html (generate-post-list-html posts-with-backlinks))
  
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
