#|
  This file is a part of networks-search project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage networks-search-test
  (:use :cl :networks-search))
(in-package :networks-search-test)

(defparameter noisy-path
  (list
   (networks-graph:node 'node-4 '(node-3 node-5))
   (networks-graph:node 'node-5 '(node-4))
   (networks-graph:node 'node-3 '(node-1 node-2))
   (networks-graph:node 'node-2 '(node-1 node-3))
   (networks-graph:node 'node-1 '())))

(let ((path (clean-path noisy-path)))
  (unless (and (= (length path) 3)
               (networks-graph:path-p path))
    (error "clean-path failed to produce a suitable path")))

(defparameter node-1 (networks-graph:node 'node-1 '(node-2 node-3)))
(defparameter node-2 (networks-graph:node 'node-2 '(node-1 node-3)))
(defparameter node-3 (networks-graph:node 'node-3 '(node-1 node-2)))
(defparameter node-4 (networks-graph:node 'node-4 '(node-5)))
(defparameter node-5 (networks-graph:node 'node-5 '(node-4)))
(defparameter temp-net (list node-1 node-2 node-3 node-4 node-5))
(when (network-connected-p temp-net)
  (error "TEMP-NET is not connected, but NETWORK-CONNECTED-P disagrees."))

(defvar ARPA-NET
  (list
   (networks-graph:node 'sri  '(utah stan ucla ucsb))
   (networks-graph:node 'ucsb '(sri  ucla))
   (networks-graph:node 'ucla '(ucsb stan  sri rand))
   (networks-graph:node 'stan '(sri  ucla))
   (networks-graph:node 'utah '(sri  sdc  mit))
   (networks-graph:node 'sdc  '(utah rand))
   (networks-graph:node 'rand '(ucla bbn))
   (networks-graph:node 'mit  '(utah bbn  linc))
   (networks-graph:node 'bbn  '(rand mit  harv))
   (networks-graph:node 'harv '(bbn  carn))
   (networks-graph:node 'carn '(harv case))
   (networks-graph:node 'case '(carn linc))
   (networks-graph:node 'linc '(case mit))))

(let ((dfs-path (dfs ARPA-NET 'sri 'harv))
      (bfs-path (bfs ARPA-NET 'sri 'harv)))
  (unless (networks-graph:path-p dfs-path)
    (error "DFS failed to find a valid path."))
  (unless (networks-graph:path-p bfs-path)
    (error "BFS failed to find a valid path."))
  (unless (network-connected-p ARPA-NET)
    (error "ARPA-NET is connected, but NETWORK-CONNECTED-P disagrees.")))

(when (local-gatekeeper-p ARPA-NET (networks-graph:get-node ARPA-NET 'ucsb))
  (error "UCSB is not a local gatekeeper."))
(unless (local-gatekeeper-p ARPA-NET (networks-graph:get-node ARPA-NET 'sdc))
  (error "SDC is a local gatekeeper."))

(format t "~%~%NETWORKS-SEARCH-TEST ok.~%")

