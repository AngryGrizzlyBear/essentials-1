;;-------------------------------------------------------------------
;; Sample private.el
;;
;; Base your private.el off of this if you want an easier time
;; setting things up.
;;
;;-------------------------------------------------------------------


;; Circe Configuration
(defvar irc-username "")
(defvar irc-password "")

(defvar irc-servers '(
   (
     "Freenode"
     :nick "Your nick"
     :channels ("channel1" "channel2")
     :nickserv-password irc-password
     :port port
     :tls t
   )
  )
)

(defvar default-real-name "Real Name")

;; Email Configuration



;; Usenet Configuration
