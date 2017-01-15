// @flow

"use strict";

const spawn = require("child_process").spawn;
const http = require("http");

/******************************************************************************/

const port = 2233

/******************************************************************************/

function spawnBackend(port/*: Number*/, succ/*: () => void*/, err/*: string => void*/) {
  const proc = spawn("./hs-main", [port.toString()], {});
  proc.on("error", err  => err("error when spawning child: " + err));
  proc.on("exit",  code => err("child exited with code: " + code));
  proc.stdout.on('data', data => { console.log(`hs-main stdout: ${data}`); });
  proc.stderr.on('data', data => { console.log(`hs-main stderr: ${data}`); });

  const start = Date.now();
  function wait() {
    const now = Date.now();
    if(now - start > 10 * 1000) {
      proc.kill()
      err("Timeout when spawning child")
    } else {
      const req = http.request({
        host: "127.0.0.1",
        port: port,
        path: "/status",
        method: "GET",
        timeout: 500,
      });
      req.on("error", msg => {
        console.log(`Tried to connect to backend, but got: ${msg}, retrying...`);
        setTimeout(wait, 100)
      });
      req.on("response", res => {
        const code = res.statusCode;
        if(code != 200) {
          console.log(`Tried to connect to backend, but got http code: ${code}, retrying...`)
          setTimeout(wait, 100)
        } else {
          console.log(`Backend is up.`)
          succ()
        }
      });
    }
  }

  setImmediate(wait)
}

/******************************************************************************/

var lastError/*: ?string */ = null;
function setError(msg/*: string */) {
  lastError = ""+msg;
}

process.on('uncaughtException', setError)

/******************************************************************************/

var backendReady = false;
function waitForBackendOrError(cb /*: ?string => void */) {
  if(lastError != null) {
    cb(lastError)
  }
  else {
    
  }
}

/******************************************************************************/

exports.handler = function( event/*: Object */, context/*: Object*/
                            , callback/*: (?any, ?Object) => void */) {
  context.callbackWaitsForEmptyEventLoop = false;
  const payload = {
    payload: event,
    context: context
  };

  if(lastError != null)
    callback(lastError, null)
  else {  
    ask(payload, answer =>
      answer.tag == "Success"
        ? callback(null, answer.contents)
        : callback(answer)
    );
  }
};

