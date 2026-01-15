;; ==========================================
;; PYTHON QUOTES
;; ==========================================

;; [python :quotes]
(string
  (string_content) @content) @around

(interpolation
  "{" @ignore
  (_) @content
  "}" @ignore) @around

;; ==========================================
;; PYTHON COLLECTIONS (Brackets)
;; ==========================================

;; [python :brackets]

;; List: [1, 2, 3]
(list
  "[" @ignore
  (_) @content
  "]" @ignore) @around

;; Dictionary: {"a": 1}
(dictionary
  "{" @ignore
  (_) @content
  "}" @ignore) @around

;; Tuple: (1, 2)
(tuple
  "(" @ignore
  (_) @content
  ")" @ignore) @around

;; Subscript: arr[0:10]
(subscript
  "[" @ignore
  (_) @content
  "]" @ignore) @around

;; Argument Lists: func(a, b)
(argument_list
  "(" @ignore
  (_) @content
  ")" @ignore) @around

;; ==========================================
;; PYTHON BLOCKS (Functions/Classes)
;; ==========================================

;; [python :blocks]

(function_definition
  body: (block) @content) @around

(class_definition
  body: (block) @content) @around

(if_statement
  consequence: (block) @content) @around
