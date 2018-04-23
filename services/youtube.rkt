#lang racket/base

(require racket/list)
(require racket/port)
(require sxml)
(require sxml/sxpath)
(require net/url)
(require html-parsing)
(require json)

(struct video-info (thumbnail-url title duration  author) #:transparent)



(define (page->sxml url-string)
  ;; Transforms a url string to sxml.
  (html->xexp
   (port->string
    (get-pure-port (string->url url-string) #:redirections 1 ))))


(define (get-video-ids-in-page yt-page-sxml)
  ;; Gets all the video ids from a youtube page converted to sxml.
  (define all-ids-hrefs ((sxpath "//a[starts-with(@href,\"/watch?v=\")]/@href/text()") yt-page-sxml ) )  
  (define all-ids (map (lambda (elem)
			 (cadr (regexp-match #rx"/watch\\?v=(.*)" elem)))
		       all-ids-hrefs))
  (remove-duplicates all-ids))



(define (thumbnail-from-video-id video-id )
  ;; Given a video id, returns the path to the thumbnail of the video.
  
  (string-append "https://i.ytimg.com/vi/"
		 video-id
		 "/hqdefault.jpg"  ))


(define (title-from-video-id video-id page-sxml)
  ;; Given a video id and the corresponding youtube page sxml, it
  ;; returns the title of the video.
  (define watch-url (string-append "/watch?v=" video-id ))
  (define xpath-string (string-append "//h3/a[@href = \"" watch-url
				      "\" and @title]/@title/text()"))  
  (car ((sxpath xpath-string  ) page-sxml)))


(define (duration-from-video-id video-id page-sxml)
  ;; Returns the duration of a vide given the video-id and the youtube
  ;; page converted to sxml.
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

(define (video-id->video-info video-id yt-sxml-page)
  ;; Returns a video-info struct given a video-id
  (with-handlers ([exn:fail?
		   (lambda (v) empty)])
    (define thumbnail-url (thumbnail-from-video-id video-id)) 
    (define title (title-from-video-id video-id yt-sxml-page))
    (define duration (duration-from-video-id video-id yt-sxml-page))
    (define-values (author author-url) (get-user-from-video-id video-id yt-sxml-page))
    (video-info thumbnail-url title duration author))
  )



(module+ showpage
  
  (require web-server/templates)
  (require web-server/servlet)
  (require web-server/servlet-env)

  (define-syntax-rule (template-response body ...)  
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8
	    body ...	    
	    )) ))

  (define (videos-info-from-page yt-page)
    ;;; Returns a list with video-info structures for all the videos
    ;;; in the page.
    (define yt-page-sxml (page->sxml yt-page) )
    (define ids (get-video-ids-in-page yt-page-sxml))
    
    (define videos-info-list
      (map (lambda (x)
	     (video-id->video-info x yt-page-sxml))
	   ids))
    
    (filter (lambda (x)
	      (not (null? x)))
	    videos-info-list) )
  

  (define (yt-main-menu req)
    (let ([vinfo-list (videos-info-from-page "https://youtube.com")   ])
      (template-response (include-template "../templates/yt_menu.html"))))

  
   (serve/servlet yt-main-menu
   		 #:launch-browser? #f
   		 #:quit? #f
		 #:server-root-path "../templates"
   		 #:servlet-path "/")
   
  )



 
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
