(use-package 'testing)

(test "the runner clock starts at the configured time and steps"
  (assert-string= "2000-01-01T00:00:00Z" (time:format-rfc3339 (time:utc-now)))
  (assert-string= "2000-01-01T00:00:01Z" (time:format-rfc3339 (time:utc-now))))

(test "every test gets a fresh clock"
  (assert-string= "2000-01-01T00:00:00Z" (time:format-rfc3339 (time:utc-now))))

(test "time-elapsed reads the runner clock"
  ; readings: t0 = 00:00:00, then time-elapsed reads 00:00:01
  (let ([t0 (time:utc-now)])
    (assert= 1 (time:duration-s (time:time-elapsed t0)))))
