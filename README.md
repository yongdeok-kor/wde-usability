# wearable_robot_evaluation_usability

웨어러블 디바이스 사용성 평가 모델

## web

roswell https://github.com/roswell/roswell/wiki/Installation#linux

```
curl -L https://github.com/roswell/roswell/releases/download/v19.08.10.101/roswell_19.08.10.101-1_amd64.deb --output roswell.deb
sudo dpkg -i roswell.deb
```

ros install clack

which clackup

```
cat <<EOF >> app.lisp
(lambda (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, Clack!")))
EOF
```

```
clackup app.lisp
Hunchentoot server is started.
Listening on localhost:5000.
```
