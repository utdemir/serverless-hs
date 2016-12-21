// @flow

const spawn = require("child_process").spawn;
const fs = require("fs");
const readline = require("readline");

/*::
type Callback<T> = T => void
type Ask = (Object, Callback<Object>) => void
*/

function throw_(msg/*: string */)/*: void */ {
  console.log("!!!!", msg);
  throw Error(msg);
};

function trace/*::<T>*/(val/*: T*/)/*: T*/ {
  return val;
}
 
function mkTemp(prefix)/*: string*/ {
  const randName = Math.floor(Math.random() * 1000000);
  return `/tmp/${prefix}-${randName}`
}

function mkFifo(cb/*: Callback<string> */)/*: void */ {
  const path = mkTemp("serverless-hs")
  console.log(`spawn(mkfifo, [${path}]`);
  const proc = spawn("mkfifo", [path]);
  proc.on("error", err => throw_("mkfifo " + path + "failed: " + err));
  proc.on("exit", code => {
    if(code == 0)
      cb(path)
    else
      throw_("mkfifo " + path + " failed with code: " + code)
  });
}

function mkReader(path/*: string*/)/*: Promise<void>*/ {
  return new Promise((fulfill, reject) => {
    const stream = fs.createReadStream(path);
    
    stream.on("open", _ => fulfill())
    stream.on("error", err => reject(err))
    stream.on("exit", err => reject(err))
  })
}



function mkReader(filepath/*: string */, cb/*: Callback<Object> */)/*: void*/ {
  console.log(`createReadStream(${filepath})`);
  const stream = fs.createReadStream(filepath);
  stream.on("error", err => throw_("error from reader: " + err));
  stream.on("end", () => throw_("reader finished."));
  stream.on("open", _ => {
    const rl = readline.createInterface({ input: stream });
    rl.on("line", line => {
        var ret = undefined;
        try {
            ret = JSON.parse(line);
        } catch(err) {
            console.log("error parsing line, ignoring: " + err);
        }
        if(ret != undefined) cb(ret);
    });
  });
}

function mkWriter(filepath/*: string*/, cb/*: Callback<Object => void> */)/*: void*/ {
  console.log(`createWriteStream(${filepath})`);
  const stream = fs.createWriteStream(filepath);
  stream.on("error", err => throw_("error from writer: " + err));
  cb(obj => { stream.write(JSON.stringify(obj) + "\n"); });
}

function spawnBackend(callback /*: Callback<Ask> */) {
  mkFifo(inputFilename =>
    mkFifo(outputFilename => {
      mkWriter(inputFilename, write => {
        console.log(`spawn(hs-main, [${inputFilename}, ${outputFilename}])`);
        const proc = spawn(
          "./hs-main", [inputFilename, outputFilename],
          { stdio: "inherit"}
        );
        proc.on("error", err  => throw_("error when spawning child: " + err));
        proc.on("exit",  code => throw_("child exited with code: " + code));

        const table/*: { [key: number]: Object => void } */ = {};
        var lastUid = 0;

        callback((payload, cb) => {
          lastUid = lastUid + 1;

          table[lastUid] = cb;

          write({
            uid: lastUid,
            contents: payload
          });
        });

        mkReader(outputFilename, ret =>
          table[ret.uid](ret.contents)
        );
      });
    })
  );
}

/******************************************************************************/

var ask/*: Ask */ = (payload, cb) => {
  console.log("Backend not initialized, retrying...");
  setTimeout(ask, 10, payload, cb);
};

spawnBackend(cb => {
  console.log("Backend initialized");
  ask = cb;
});

/******************************************************************************/

exports.handler = function( event/*: Object */, context/*: Object*/
                            , callback/*: (?Object, ?Object) => void */) {
  context.callbackWaitsForEmptyEventLoop = false;
  const payload = {
    payload: event,
    context: context
  };
  ask(payload, answer =>
    answer.tag == "Success"
      ? callback(null, answer.contents)
      : callback(answer)
  );
};

