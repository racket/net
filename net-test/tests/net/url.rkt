#lang racket
(require net/url
         tests/eli-tester
         racket/sandbox)

(provide tests)
(module+ main (test do (tests)))
(define (tests)

  (define envar-stash (environment-variables-copy (current-environment-variables)))
  
  (define (test-proxy-server-for schema
                                 (host #f)
                                 #:plt-http-proxy (plt-http-proxy "")
                                 #:http-proxy (http-proxy "")
                                 #:plt-no-proxy (plt-no-proxy "")
                                 #:no-proxy (no-proxy ""))
    (parameterize ([current-environment-variables envar-stash])
      (define sbx (make-evaluator
                   `(begin
                      (require net/url)
                      (putenv "plt_http_proxy" ,plt-http-proxy)
                      (putenv "http_proxy" ,http-proxy)
                      (putenv "plt_no_proxy" ,plt-no-proxy)
                      (putenv "no_proxy" ,no-proxy))))
      (sbx `(parameterize
                ([current-proxy-servers (quote ,(current-proxy-servers))]
                 [current-no-proxy-servers (quote ,(current-no-proxy-servers))])
              (proxy-server-for ,schema ,host)))))
    
  (test
   ;; Test the current-proxy-servers parameter can be set
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (current-proxy-servers))
   => '(("http" "proxy.com" 3128))

   ;; we have at least http
   (member "http" proxiable-url-schemes)

   ;; by default, there are no proxy servers
   (test-proxy-server-for "http") => #f

   ;; current-no-proxy-servers converts incoming strings to anchored regexps
   (parameterize ([current-no-proxy-servers (list "test.racket-lang.org"
                                              #rx".*\\.racket-lang\\.org")])
     (current-no-proxy-servers))
   => '(#rx"^test\\.racket-lang\\.org$"
        #rx".*\\.racket-lang\\.org")

   ;; ------------------------------------------------------------------
   ;; Test Proxy Servers (loading from environment and proxy-server-for)
   
   ;; proxy servers set in current-proxy-servers are not overridden by environment
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (test-proxy-server-for #:plt-http-proxy "http://proxy.net:1234"
                            #:http-proxy "http://proxy.net:1234"
                            "http" "test.racket-lang.org"))
   => '("http" "proxy.com" 3128)

   ;; plt_http_proxy is is prioritised over http_proxy
   (test-proxy-server-for #:plt-http-proxy "http://proxy.net:3128"
                          #:http-proxy "http://proxy.net:3228"
                          "http" "test.racket-lang.org")
   => '("http" "proxy.net" 3128)

   ;; otherwise fall back to http_proxy
   (test-proxy-server-for #:http-proxy "http://proxy.net:3228"
                          "http" "test.racket-lang.org")
   => '("http" "proxy.net" 3228)

   ;; ---------------------------------------------------------------------
   ;; Test NO Proxy Servers (loading from environment and proxy-server-for)
   ;; no proxy servers accumulate (they don't override), so test each one
   ;; being inserted in turn

   ;; prove that we need a proxy if not otherwise told...
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (test-proxy-server-for "http" "test.racket-lang.org"))
   => '("http" "proxy.com" 3128)
   
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))]
                  [current-no-proxy-servers '("test.racket-lang.org")])
     (test-proxy-server-for "http" "test.racket-lang.org"))
   => #f
   
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (test-proxy-server-for #:plt-no-proxy "test.racket-lang.org"
                            "http" "test.racket-lang.org"))
   => #f
   
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (test-proxy-server-for #:no-proxy "test.racket-lang.org"
                            "http" "test.racket-lang.org"))
   => #f

   ;; Pattern matching
   ;; prove that we need a proxy if not otherwise told...
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))]
                  [current-no-proxy-servers '(#rx".racket-lang.org")])
     (test-proxy-server-for "http" "test.racket-lang.org"))
   => #f
   
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (test-proxy-server-for #:plt-no-proxy ".racket-lang.org"
                            "http" "test.racket-lang.org"))
   => #f
   
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (test-proxy-server-for #:no-proxy ".racket-lang.org"
                            "http" "test.racket-lang.org"))
   => #f

   ;; Failed matches
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (test-proxy-server-for #:plt-no-proxy ".racket-lang.org"
                            "http" "test.bracket-lang.org"))
   => '("http" "proxy.com" 3128)
   
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (test-proxy-server-for #:no-proxy ".racket-lang.org"
                            "http" "test.bracket-lang.org"))
   => '("http" "proxy.com" 3128)

  ;; Look at this... the no-proxes has a regexp which starts with a '.', a regexp
  ;; any char... that will match the 'b' in bracket
  (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))]
                 [current-no-proxy-servers '(#rx".racket-lang.org")])
    (test-proxy-server-for "http" "test.bracket-lang.org"))
   => #f
  ))

(module+ test (require (submod ".." main))) ; for raco test & drdr
