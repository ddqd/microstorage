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

#### PURE GET API
#####GET
`get /api/?method=GET&uuid=UUID&key=KEY`   

#####SET
`get /api/?method=SET&uuid=UUID&key=KEY&data={"k":"v","k2":"v2"}`  

#####DELETE  
`get /api/?method=DELETE&uuid=UUID&key=KEY`

--- 

####API
`get /api/?method=GET&uuid=UUID&key=KEY` 

`delete /api/uuid=123&key=123`

`post  /api/   {"uuid":"123", "key":"KEY", "data":{"key":"value", "key1":"value1"}}`

---
####Example
#####add
`curl -H 'Accept: application/json' -H 'Content-Type: application/json' -d '{"uuid":"a1", "key":"a", "data":{"key":"value", "key1":"value1"}}' -X POST "localhost:8081/api/"`
 `-> {"status":"ok"}`

#####get 
`curl "localhost:8081/api/?method=GET&uuid=UUID&key=KEY`
`-> {"status":"ok","uuid":"UUID","key":"KEY","data":{"key":"value","key1":"value1"}}`

#####delete
`curl -X DELETE "localhost:8081/api/?uuid=a1&key=a"`
`{"status":"ok"}`



> Written with [StackEdit](https://stackedit.io/).
