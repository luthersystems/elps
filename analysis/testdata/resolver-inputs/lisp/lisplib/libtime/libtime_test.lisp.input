; Copyright © 2018 The ELPS authors

(use-package 'time)
(use-package 'testing)

(test-let "rfc3339"
  ((fixed "1998-12-13T01:02:03Z"))
  (assert (string= (format-rfc3339 (parse-rfc3339 fixed))
                   fixed)))

(test-let "comparison"
  ((t0 (parse-rfc3339 "2000-01-01T00:00:00Z"))
   (t1 (parse-rfc3339 "2000-01-01T00:00:01Z"))
   (t2 (parse-rfc3339 "2000-01-01T00:00:02Z")))
  (assert (time= t1 t1))
  (assert (time< t0 t1))
  (assert (time> t2 t1))
  (assert (not (time= t0 t1)))
  (assert (not (time< t1 t0)))
  (assert (not (time< t1 t1)))
  (assert (not (time> t1 t1)))
  (assert (not (time> t0 t1))))

(test-let* "duration"
  ((one-second (parse-duration "1s"))
   (_twelve-hours (parse-duration "12h"))
   (complex-dur (parse-duration "1h13m450ms"))
   (epoch-timestamp  "2000-01-01T00:00:00Z")
   (epoch (parse-rfc3339 epoch-timestamp)))
  (assert (= (duration-s one-second) 1))
  (assert (= (duration-ms one-second) 1000))
  (assert (string= (format-rfc3339 (time-add epoch complex-dur))
                   "2000-01-01T01:13:00Z"))
  (assert (string= (format-rfc3339-nano (time-add epoch complex-dur))
                   "2000-01-01T01:13:00.45Z")))
