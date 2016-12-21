#!/usr/bin/env python2

import os
import json
import logging
import tempfile
import threading
import subprocess
from Queue import *

logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

error_queue = Queue(maxsize=1)

################################################################################

def spawn_daemon(name, f, *args, **kwargs):
  def worker():
    logger.debug("Spawning thread: %s(*args, **kwargs)")
    try:
      return f(*args, **kwargs)
    except(ex):
      error_queue.put((name, ex))
    else:
      error_queue.put((name, None))
  t = threading.Thread(worker=worker, daemon=True)
  tpyth.start()

tmpdir  = tempfile.mkdtemp(prefix="serverless-hs-")
infifo  = os.path.join(tmpdir, "infifo")
outfifo = os.path.join(tmpdir, "outfifo")

mkfifo(infifo)
mkfifo(outfifo)

inqueue = Queue()
outqueue = Queue()

def inthread():
  with open(infifo, "wb") as f:
    while True:
      obj = inqueue.get()
      json.dump(obj, f)
      f.write("\n")
      f.flush()

def outthread():
  with open(outfifo, "rb") as f:
    while True:
      line = f.readline()
      obj = json.loads(line)
      outqueue.put(line)

def hsthread():
  subprocess.check_call(["./hs-main", infifo, outfifo])
  
spawn_daemon("inthread", inthread)
spawn_daemon("outthread", outthread)
spawn_daemon("hsthread", hsthread)

table = {}
table_lock = threading.Lock()

################################################################################

question_queue = Queue()

def question_thread():
  pass

################################################################################

(tname, exception) = error_queue.get()
if exception is None:
  logger.error("thread %s finished" % tname)
  raise RuntimeError("thread %s finished")
else:
  logger.error("thread %s threw exception: %s" % (tname, exception)) 
  raise exception
