#lang racket/base

(require racket/list)
(require racket/port)
(require sxml)
(require sxml/sxpath)
(require net/url)
(require html-parsing)
(require json)

(struct video-info (thumbail-url title  author) #:transparent)


(define (page->sxml url-string)  
  (html->xexp
   (port->string
    (get-pure-port (string->url url-string) #:redirections 1 ))))

;; (define (page->file page-url file)
;;   (with-output-to-file file
;;     (lambda ()
;;       (print (srl:sxml->html (page->sxml page-url)))
;;       )
;;     #:exists 'replace
;;     ))


;; (define (get-video-json video-id)
;;   (read-json
;;    (get-pure-port (string->url
;; 		   (format "https://www.youtube.com/oembed?format=json&url=https://www.youtube.com/watch?v=~a" video-id)) )))

(define (get-video-ids-in-page url-string)
  (define yt-page-xexp (page->sxml url-string))
  (define all-hrefs ((sxpath '(// a @ href)) yt-page-xexp) )
  (define ids-with-false
    (map   
     (lambda (elem)
       (define path (cadr elem))
       (define aux (regexp-match #rx"/watch\\?v=(.*)" path))
       (if aux (cadr aux) aux))
   
     all-hrefs
     ))
  
  (filter (lambda (x) x) ids-with-false))


;; (define (id->video-info video-id)
;;   (define video-info-json (get-video-json video-id))
;;   (video-info (hash-ref video-info-json 'thumbnail_url)
;; 	      (hash-ref video-info-json 'title)
;; 	      (hash-ref video-info-json 'author_name)))

;; (define (get-page-info yt-page)
;;   (define s-page (page->sxml yt-page))
;;   (car ((sxpath "//ytd-grid-video-renderer/") s-page))
;;   )

;; (define (get-videos-info-in-page url-string)
;;   (map id->video-info (get-video-ids-in-page url-string)))


;(define a (page->sxml "https://youtube.com"))

;(srl:sxml->html a "/tmp/output")

;; (with-output-to-file "/tmp/output"
;;   (lambda ()
;;     (print a  )
;;     ))

;; (define b
;;   ((sxpath "//a[starts-with(@href,\"/watch?\") and @title]/@title  | //a[starts-with(@href,\"/watch?\")]/div/span/img/@data-thumb | //a[starts-with(@href,\"/watch?\")]/div/span/span[@class=\"video-time\"]/text() ") a))

(define (thumbnail-from-video-id video-id )
  (string-append "https://i.ytimg.com/vi/"
		 video-id
		 "/hqdefault.jpg"  ))


(define (title-from-video-id video-id page-sxml)
  (define watch-url (string-append "/watch?v=" video-id ))
  (define xpath-string (string-append "//h3/a[@href = \"" watch-url
				      "\" and @title]/@title/text()"))  
  ((sxpath xpath-string  ) page-sxml))


(define (duration-from-video-id video-id page-sxml)
  (define watch-url (string-append "/watch?v=" video-id ))
  (define xpath-string (string-append "//h3[a/@href = \"" watch-url
				      "\"]/span/text()"))
  (define duration-string (car ((sxpath xpath-string  ) page-sxml))) 
  (cadr (regexp-match* #rx"[0-9:]+" duration-string )))


(define (get-user-from-video-id video-id page-sxml)
  ;; Returns two values, username and link
  (define watch-url (string-append "/watch?v=" video-id ))
  (define xpath-string-username (string-append "//div[h3/a/@href = \"" watch-url
					       "\"]/div/a"))  
  (define complete-name-link-node `(*TOP* ,(car ((sxpath xpath-string-username) page-sxml))))
  (define username (car ((sxpath "/a/text()") complete-name-link-node )))
  (define username-link (car ((sxpath "/a/@href/text()") complete-name-link-node )) )
;  ((sxpath "//a/text()") complete-username-link-sxml )
  (values username username-link))



  




;; ((sxpath "//a[div/span[@class]") a)

;; (define b ((sxpath "//a[starts-with(@href,\"/watch?\") and div/span]") a))

;; ((sxpath "//@href | //img/@data-thumb | //span[@class=\"video-time\"]") b)

  ;; ;((sxpath "//a[starts-with(@href,\"/watch?\") and div/span]/@href | ") a)
  
	

(module+ trials
  
  (require sxml)

  (require web-server/servlet-env)
  
  (require web-server/http/response-structs)

  (define-syntax-rule (template-response body ...)  
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8
	    body ...	    
	    )) )  
    )

  (define page1 '(*TOP* (html
			 (body
			  (a (@ (href "/watch?=code")))
			  (a (@ (href "lala")))
			  (a (@ (href "/lala")))
			  (h1 "This is a test")))))

					; Cases that start with / but do not start with /watch
  
  ((sxml:modify (list "//a[not(starts-with(@href,\"/watch\")) and starts-with(@href,\"/\") ]" 'delete)) page1)

  ((sxml:modify (list "//a[not(starts-with(@href,\"/watch\")) and starts-with(@href,\"/\")]/@href/text()"
		      (lambda (node ctx root)
			(string-append "https://youtube.com" node)
			)
		      )		
		) page1)

  (define (addapt-yt-page yt-page)

    (define (change-css-links yt-page)
      ((sxml:modify (list "//link[starts-with(@href,\"/\") ]/@href/text()"
			  (lambda (node ctx root)
			    (string-append "https://youtube.com" node))))
       yt-page) )
    
    (define (change-js-links yt-page)      
      ((sxml:modify (list "//script[starts-with(@src,\"/\")]/@src/text()"
			  (lambda (node ctx root)
			    (string-append "https://youtube.com" node))))       
       yt-page))

    (define (change-relative-paths yt-page)
      ((sxml:modify (list "//*[starts-with(@*,\"/\")]/@*/text()"
			  (lambda (node ctx root)
			    (string-append "https://youtube.com" node))))
       yt-page
       ))
    
    

					;    (change-js-links (change-css-links yt-page)  )    
    (change-relative-paths yt-page)
    )

  (define (change-relative-paths yt-page)
    ((sxml:modify (list "//*[starts-with(@*,\"/\")]/@*/text()"
			(lambda (node ctx root)
			  (string-append "https://youtube.com" node))))
     yt-page
     ))
  

  (define (start req)
    (template-response
     (srl:sxml->html
      (change-relative-paths (page->sxml "https://www.youtube.com" )))
     
     ))
  
  (serve/servlet start
		 #:launch-browser? #f
		 #:quit? #f
		 #:servlet-path "/")
    
  )
  


(module+ test
  (require rackunit)
  (require sxml)
  (require web-server/servlet-env)
  
  (require web-server/http/response-structs)
  

  (define page1 '(*TOP* (html
			 (body
			  (a (@ (href "/watch?=code")))
			  (h1 "This is a test")))))

  ;; ( (sxml:modify
  ;;   (list "//a[starts-with(@href,\"/watch?\")]" 'replace ))   
  ;;  page1 )


  

  (define-syntax-rule (template-response body ...)  
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8
	    body ...	    
	    )) )  
    )

 
  (define (start req)
    (template-response
     (srl:sxml->html  (page->sxml "http://www.youtube.com" ) )
     ))


  
  
  
  )
