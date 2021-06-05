; Copyright © 2018 The ELPS authors

(use-package 'testing)

(test "use-string-numbers"
  (assert-string= "\"1\"" (to-string (json:message-bytes (json:dump-message 1 :string-numbers true))))
  (assert-string= "\"1\"" (json:dump-string 1 :string-numbers true))
  (assert-string= "1" (json:load-string "1" :string-numbers true))

  ; Assert that, when :string-numbers is not given (or nil) the setting
  ; defaults back to the last call of use-string-numbers (or false if never
  ; called)
  (assert-string= "1" (json:dump-string 1 :string-numbers ()))
  (assert= 1 (json:load-string "1" :string-numbers ()))
  (assert-string= "1" (json:dump-string 1))
  (assert= 1 (json:load-string "1"))
  (assert-nil (json:use-string-numbers true))
  (assert-string= "\"1\"" (json:dump-string 1 :string-numbers ()))
  (assert-string= "1" (json:load-string "1" :string-numbers ()))
  (assert-string= "\"1\"" (json:dump-string 1))
  (assert-string= "1" (json:load-string "1"))
  ; the value false can be used to override a true setting of
  ; use-string-numbers
  (assert-string= "1" (json:dump-string 1 :string-numbers false))
  (assert= 1 (json:load-string "1" :string-numbers false))
  (assert-string= "1" (json:load-message (json:dump-message 1 :string-numbers false)
                                         :string-numbers true))
  (assert= 1 (json:load-message (json:dump-message 1 :string-numbers false)
                                :string-numbers false))
  (assert-string= "1" (json:load-message (json:dump-message 1 :string-numbers true)
                                         :string-numbers false))
  (assert-string= """{"data":"1","id":1}"""
                  (json:dump-string (sorted-map "id"    1
                                                "data"  (json:dump-message 1 :string-numbers true))
                                    :string-numbers false))
  )

(test "marshal"
  (assert-string= """null"""
                  (json:dump-string ()))
  (assert-string= """true"""
                  (json:dump-string true))
  (assert-string= """false"""
                  (json:dump-string false))
  (assert-string= """12"""
                  (json:dump-string 12))
  (assert-string= """[1,2]"""
                  (json:dump-string '(1 2)))
  (assert-string= """[1,2]"""
                  (json:dump-string [1 2]))
  (assert-string= """[1,2]"""
                  (json:dump-string (vector 1 2)))
  (assert-string= """{}"""
                  (json:dump-string (sorted-map)))
  (assert-string= """{"a":1}"""
                  (json:dump-string (sorted-map "a" 1)))
  (assert-string= """{"a":1,"b":2}"""
                  (json:dump-string (sorted-map "a" 1 "b" 2)))
  (assert-string= """{"a":{"b":"c"}}"""
                  (json:dump-string (sorted-map "a" (sorted-map "b" "c")))))

(test "unmarshal"
  (set 'js-val (json:load-string """null"""))
  (assert-nil js-val)
  (set 'js-val (json:load-string """true"""))
  (assert js-val)
  (assert-string= "true" (to-string js-val))
  (set 'js-val (json:load-string """false"""))
  (assert-not js-val)
  (assert-not-nil js-val)
  (assert-string= "false" (to-string js-val))
  (set 'js-val (json:load-string """4.0"""))
  (assert= 4 js-val)
  (set 'js-val (json:load-string """[1, 2, 3]"""))
  (assert-equal (vector 1 2 3) js-val)
  (set 'js-val (json:load-string """[]"""))
  (assert-equal (vector) js-val)
  (set 'js-val (json:load-string """[[]]"""))
  (assert-equal (vector (vector)) js-val)
  (set 'js-val (json:load-string """{"a":[], "b":null,"c":{}}"""))
  (assert (sorted-map? js-val))
  (assert-equal '("a" "b" "c") (keys js-val))
  (assert-equal (vector) (get js-val "a"))
  (assert-nil (get js-val "b"))
  (assert (sorted-map? (get js-val "c")))
  (assert= 0 (length (keys (get js-val "c")))))

(test "unmarshal-syntax-error"
  (assert-string= "syntax-error"
                  (handler-bind ([json:syntax-error (lambda (c _) "syntax-error")])
                    (json:load-string "{false:true}")))
  (assert-string= "ok-json"
                  (handler-bind ([json:syntax-error (lambda (c _) "syntax-error")])
                    (json:load-string "\"ok-json\""))))

(benchmark-simple "load-object"
  (let ([b (to-bytes """{"test1": 123, "test2": 456, "test3": 789}""")])
    (dotimes (n 1000)
      (json:load-bytes b))))

(benchmark-simple "load-array"
  (let ([b (to-bytes """["test1", 123, "test2", 456, "test3", 789]""")])
    (dotimes (n 1000)
      (json:load-bytes b))))

(benchmark-simple "load-nested"
  (let ([b (to-bytes """{"test1": 123, "test2": 456, "test3": {"test1": 123, "test2": 456, "test3": 789}}""")])
    (dotimes (n 1000)
      (json:load-bytes b))))

(benchmark-simple "load-github"
  (let ([b (to-bytes github-json)])
    (dotimes (n 1000)
      (json:load-bytes b))))

(set 'benchmark-input-get-nested """
{
  "e0": 12,
  "e1":
  {
    "e0": 34
  },
  "e2":
  [
    {
      "e0": 56
    }
  ],
  "e3":
  [
    {
      "e0":
      {
        "e0": 78
      },
      "e1":
      [
        {
          "e0": 90
        }
      ]
    }
  ]
}
""")

(benchmark-simple "get-nested-baseline"
  (let ([v (json:load-bytes (to-bytes benchmark-input-get-nested))])
    (dotimes (n 1000)
      (assert-equal 12
                    (thread-first v
                                  (get "e0")))
      (assert-equal 34
                    (thread-first v
                                  (get "e1")
                                  (get "e0")))
      (assert-equal 56
                    (thread-first v
                                  (get "e2")
                                  (nth 0)
                                  (get "e0")))
      (assert-equal 78
                    (thread-first v
                                  (get "e3")
                                  (nth 0)
                                  (get "e0")
                                  (get "e0")))
      (assert-equal 90
                    (thread-first v
                                  (get "e3")
                                  (nth 0)
                                  (get "e1")
                                  (nth 0)
                                  (get "e0")))
      )))

(benchmark-simple "dump-object"
  (let* ([val (sorted-map "test1" 123 "test2" 456 "test3" 789)])
    (dotimes (n 1000)
      (json:dump-string val))))

(benchmark-simple "dump-array"
  (let* ([val (list "test1" 123 "test2" 456 "test3" 789)])
    (dotimes (n 1000)
      (json:dump-string val))))

(benchmark-simple "dump-nested"
  (let* ([val (sorted-map "test1" 123 "test2" 456 "test3" (sorted-map "test1" 123 "test2" 456 "test3" 789))])
    (dotimes (n 1000)
      (json:dump-string val))))

(benchmark-simple "dump-github"
  (let* ([val (json:load-string github-json)])
    (dotimes (n 1000)
      (json:dump-string val))))

; curl https://api.github.com/repos/luthersystems/elps  
(set 'github-json """
{
  "id": 158678640,
  "node_id": "MDEwOlJlcG9zaXRvcnkxNTg2Nzg2NDA=",
  "name": "elps",
  "full_name": "luthersystems/elps",
  "private": false,
  "owner": {
    "login": "luthersystems",
    "id": 20160060,
    "node_id": "MDEyOk9yZ2FuaXphdGlvbjIwMTYwMDYw",
    "avatar_url": "https://avatars.githubusercontent.com/u/20160060?v=4",
    "gravatar_id": "",
    "url": "https://api.github.com/users/luthersystems",
    "html_url": "https://github.com/luthersystems",
    "followers_url": "https://api.github.com/users/luthersystems/followers",
    "following_url": "https://api.github.com/users/luthersystems/following{/other_user}",
    "gists_url": "https://api.github.com/users/luthersystems/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/luthersystems/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/luthersystems/subscriptions",
    "organizations_url": "https://api.github.com/users/luthersystems/orgs",
    "repos_url": "https://api.github.com/users/luthersystems/repos",
    "events_url": "https://api.github.com/users/luthersystems/events{/privacy}",
    "received_events_url": "https://api.github.com/users/luthersystems/received_events",
    "type": "Organization",
    "site_admin": false
  },
  "html_url": "https://github.com/luthersystems/elps",
  "description": "An embedded lisp interpreter",
  "fork": false,
  "url": "https://api.github.com/repos/luthersystems/elps",
  "forks_url": "https://api.github.com/repos/luthersystems/elps/forks",
  "keys_url": "https://api.github.com/repos/luthersystems/elps/keys{/key_id}",
  "collaborators_url": "https://api.github.com/repos/luthersystems/elps/collaborators{/collaborator}",
  "teams_url": "https://api.github.com/repos/luthersystems/elps/teams",
  "hooks_url": "https://api.github.com/repos/luthersystems/elps/hooks",
  "issue_events_url": "https://api.github.com/repos/luthersystems/elps/issues/events{/number}",
  "events_url": "https://api.github.com/repos/luthersystems/elps/events",
  "assignees_url": "https://api.github.com/repos/luthersystems/elps/assignees{/user}",
  "branches_url": "https://api.github.com/repos/luthersystems/elps/branches{/branch}",
  "tags_url": "https://api.github.com/repos/luthersystems/elps/tags",
  "blobs_url": "https://api.github.com/repos/luthersystems/elps/git/blobs{/sha}",
  "git_tags_url": "https://api.github.com/repos/luthersystems/elps/git/tags{/sha}",
  "git_refs_url": "https://api.github.com/repos/luthersystems/elps/git/refs{/sha}",
  "trees_url": "https://api.github.com/repos/luthersystems/elps/git/trees{/sha}",
  "statuses_url": "https://api.github.com/repos/luthersystems/elps/statuses/{sha}",
  "languages_url": "https://api.github.com/repos/luthersystems/elps/languages",
  "stargazers_url": "https://api.github.com/repos/luthersystems/elps/stargazers",
  "contributors_url": "https://api.github.com/repos/luthersystems/elps/contributors",
  "subscribers_url": "https://api.github.com/repos/luthersystems/elps/subscribers",
  "subscription_url": "https://api.github.com/repos/luthersystems/elps/subscription",
  "commits_url": "https://api.github.com/repos/luthersystems/elps/commits{/sha}",
  "git_commits_url": "https://api.github.com/repos/luthersystems/elps/git/commits{/sha}",
  "comments_url": "https://api.github.com/repos/luthersystems/elps/comments{/number}",
  "issue_comment_url": "https://api.github.com/repos/luthersystems/elps/issues/comments{/number}",
  "contents_url": "https://api.github.com/repos/luthersystems/elps/contents/{+path}",
  "compare_url": "https://api.github.com/repos/luthersystems/elps/compare/{base}...{head}",
  "merges_url": "https://api.github.com/repos/luthersystems/elps/merges",
  "archive_url": "https://api.github.com/repos/luthersystems/elps/{archive_format}{/ref}",
  "downloads_url": "https://api.github.com/repos/luthersystems/elps/downloads",
  "issues_url": "https://api.github.com/repos/luthersystems/elps/issues{/number}",
  "pulls_url": "https://api.github.com/repos/luthersystems/elps/pulls{/number}",
  "milestones_url": "https://api.github.com/repos/luthersystems/elps/milestones{/number}",
  "notifications_url": "https://api.github.com/repos/luthersystems/elps/notifications{?since,all,participating}",
  "labels_url": "https://api.github.com/repos/luthersystems/elps/labels{/name}",
  "releases_url": "https://api.github.com/repos/luthersystems/elps/releases{/id}",
  "deployments_url": "https://api.github.com/repos/luthersystems/elps/deployments",
  "created_at": "2018-11-22T10:01:06Z",
  "updated_at": "2021-05-16T03:55:18Z",
  "pushed_at": "2021-05-16T03:55:15Z",
  "git_url": "git://github.com/luthersystems/elps.git",
  "ssh_url": "git@github.com:luthersystems/elps.git",
  "clone_url": "https://github.com/luthersystems/elps.git",
  "svn_url": "https://github.com/luthersystems/elps",
  "homepage": null,
  "size": 2892,
  "stargazers_count": 13,
  "watchers_count": 13,
  "language": "Go",
  "has_issues": true,
  "has_projects": true,
  "has_downloads": true,
  "has_wiki": true,
  "has_pages": true,
  "forks_count": 7,
  "mirror_url": null,
  "archived": false,
  "disabled": false,
  "open_issues_count": 3,
  "license": {
    "key": "bsd-3-clause",
    "name": "BSD 3-Clause \"New\" or \"Revised\" License",
    "spdx_id": "BSD-3-Clause",
    "url": "https://api.github.com/licenses/bsd-3-clause",
    "node_id": "MDc6TGljZW5zZTU="
  },
  "forks": 7,
  "open_issues": 3,
  "watchers": 13,
  "default_branch": "master",
  "temp_clone_token": null,
  "organization": {
    "login": "luthersystems",
    "id": 20160060,
    "node_id": "MDEyOk9yZ2FuaXphdGlvbjIwMTYwMDYw",
    "avatar_url": "https://avatars.githubusercontent.com/u/20160060?v=4",
    "gravatar_id": "",
    "url": "https://api.github.com/users/luthersystems",
    "html_url": "https://github.com/luthersystems",
    "followers_url": "https://api.github.com/users/luthersystems/followers",
    "following_url": "https://api.github.com/users/luthersystems/following{/other_user}",
    "gists_url": "https://api.github.com/users/luthersystems/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/luthersystems/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/luthersystems/subscriptions",
    "organizations_url": "https://api.github.com/users/luthersystems/orgs",
    "repos_url": "https://api.github.com/users/luthersystems/repos",
    "events_url": "https://api.github.com/users/luthersystems/events{/privacy}",
    "received_events_url": "https://api.github.com/users/luthersystems/received_events",
    "type": "Organization",
    "site_admin": false
  },
  "network_count": 7,
  "subscribers_count": 6
}
""")
