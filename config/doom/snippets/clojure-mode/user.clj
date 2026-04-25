# -*- mode: snippet -*-
# name: user.clj
# key: __user.clj
# condition: t
# --
(ns user
  (:require [wing.repl :as repl]))

(comment
  (repl/sync-libs!))