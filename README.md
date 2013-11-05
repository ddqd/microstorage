##Erlang microstorage
---
####dependencies

* Erlang/OTP
* git
* GNU Make
* GNU Screen

---
####Usage

1. ` git clone https://github.com/ddqd/microstorage.git`
2. `cd microstorage`
3. `make bootstrap`
4. `screen -ls` ->`PID.microstorage`
5. `screen -R PID.microstorage`

---
####API

`get /api/uuid=123&name=123`

`delete /api/uuid=123&name=123`

`post  /api/   {"uuid":"123", "name":"name", "data":[{"key":"value"}, {"key1":"value1"}]}`

---
####Example
#####add
`curl -H 'Accept: application/json' -H 'Content-Type: application/json' -d '{"uuid":"a1", "name":"a", "data":{"key":"value", "key1":"value1"}}' -X POST "localhost:8081/api/"`
 `-> {"status":"ok"}`

#####get 
`curl "localhost:8081/api/?uuid=a1&name=a"`
`-> {"status":"ok","uuid":"a1","name":"a","data":{"key":"value","key1":"value1"}}`

#####delete
`curl -X DELETE "localhost:8081/api/?uuid=a1&name=a"`
`{"status":"ok"}`


> Written with [StackEdit](https://stackedit.io/).
