(setq bibo/ical-source-list (list (cfw:ical-create-source "Oracle" "http://stbeehive.oracle.com/caldav/st/home/bibo.zou%40oracle.com/icalendar/MyCalendar.ics" "Orange")
                                  (cfw:ical-create-source "MS Live" "https://sharing.calendar.live.com/calendar/private/6bb9c89d-34e2-41b0-96a7-61e8814311db/192f9239-5695-46e2-9741-70bbd842bed3/cid-4076c1738475ecf1/calendar.ics" "Yellow")))

(when *is-linux*
  (setq racer-rust-src-path "/mnt/Pacific/development/rustc-1.5.0/src")
  (setq racer-cmd "/home/hekinami/.cargo/bin/racer")
  )

(when *is-windows*
  (setq racer-rust-src-path "D:/development/rustc-1.9.0/src")
  (setq racer-cmd "D:/HOME/.cargo/bin/racer")
  )

(setq wl-smtp-connection-type 'ssl
      wl-smtp-posting-port 465
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "zoubibo@amiunique.net"
      wl-smtp-posting-server "smtp.exmail.qq.com"
      wl-local-domain "smtp.exmail.qq.com"
      wl-message-id-domain "smtp.exmail.qq.com")
