#lang core-lisp

(core-lisp:import m :from "exports.lsp"
  :binding (x (cl:function y) (:macro z)))

(:export-default (list x (y) (z)))
