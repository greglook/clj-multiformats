(ns multiformats.test-runner
  (:require
    [doo.runner :refer-macros [doo-tests]]
    ;[multiformats.base-test]
    [multiformats.varint-test]))


(doo-tests
  ;'multiformats.base-test
  'multiformats.varint-test)
