(ns morse.core)

(defn morse-decode [code]
  (nth "54 3   2       16       7   8 90hvf l pjbxcyzq  surwdkgoianmet"
    (bit-and 63
      (reduce (fn [c b] (+ (bit-shift-left c 1) (case b \. 0 \- 1))) -2 code))))


(morse-decode ".-") ;; \a
(morse-decode "-.-") ;; \k
(morse-decode "-----") ;; \0

(comment
  ;; inspirations: https://nullprogram.com/blog/2020/12/31/

  ;; How does it work?
  ;; Morse is encoded with 2 symbols `.` (dot) and `-` (dash).
  ;; e.g: a = .- and 0 = -----
  ;; Since there are only 2 symbols we can easily map
  ;; one to 0 and the other to 1

  ;; Let's pick 0 for . and 1 for -
  ;; Morse code is encoded on up to 5 bits (between 2 and 5)

  ;; Because of the variable length encoding, we need
  ;; a way to know the length of the code

  ;; Examples:
  ;; a = .- = 01
  ;; u = ..- = 001
  ;; Problem: how to differentiate a from u?
  ;; Let's try a 1 padding on 8 bits (same goes with 0s)
  ;; a = .-  =  01 = 1111 1101
  ;; u = ..- = 001 = 1111 1001
  ;; nice but `fake news` because x = -..- = 1001 = 1111 1001 = u (but u != x)

  ;; 1st conclusion: if we pad with 1s we can't know where
  ;; codes starting by 1 do start, if we pad with 0s we can't
  ;; know where codes starting by 0 do start

  ;; We need a way to know where padding ends.
  ;; So the padding can't be uniform (only 1s or 0s)
  ;; If we flip the last bit of the padding, we know that the
  ;; actual morse code starts after the first flip
  ;; We can either try 0000 0001 or 1111 1110
  ;; Let's try 1111 1110
  ;; a = .-     = 01 = 1111 1001
  ;; u = ..-   = 001 = 1111 0001
  ;; x = -..- = 1001 = 1110 1001
  ;; x != u Hooray

  ;; So let's set the padding to 1111 1110 = -2

  ;; For a = .- = 01 = 1111 1001
  ;; We start with our padding
  ;; start = 1111 1110
  ;; bit-shift-left of the padding
  ;; 1111 1110 << 1 = 1111 1100
  ;; We add . = 0
  ;; 1111 1100 + 0000 0000 = 1111 1100
  ;; bit-shift-left of last result
  ;; 1111 1100 << 1 = 1111 1000
  ;; We add - = 1
  ;; 1111 1000 + 0000 0001 = 1111 1001
  ;; Indeed a = .- = 01 = 1111 1001

  ;; In Clojure it would be
  (reduce (fn [s c] (+ (bit-shift-left s 1) (case c \. 0 \- 1))) -2 code)

  ;; To keep only the 6 lowest bits (since higher bits are all 1s)
  2r111111 ;; 63

  ;; Result of the index fn
  (bit-and 63 (reduce (fn [s c] (+ (bit-shift-left s 1) (case c \. 0 \- 1))) -2 code))

  ;; Given a morse code, this fn returns an index (0..63)
  ;; We now need to build the corresponding lookup table

  ;; Considering the morse dictionary (source: wikidia)

  (def dict {".-" "a"
             "-..." "b"
             "-.-." "c"
             "-.." "d"
             "." "e"
             "..-." "f"
             "--." "g"
             "...." "h"
             ".." "i"
             ".---" "j"
             "-.-" "k"
             ".-.." "l"
             "--" "m"
             "-." "n"
             "---" "o"
             ".--." "p"
             "--.-" "q"
             ".-." "r"
             "..." "s"
             "-" "t"
             "..-" "u"
             "...-" "v"
             ".--" "w"
             "-..-" "x"
             "-.--" "y"
             "--.." "z"
             ".----" "1"
             "..---" "2"
             "...--" "3"
             "....-" "4"
             "....." "5"
             "-...." "6"
             "--..." "7"
             "---.." "8"
             "----." "9"
             "-----" "0"})

  ;; for each morse code we produce a map of {index char}
  (def indices->char (into {} (map #(vector (index (key %)) (val %))) dict))

  ;; indices->char =
  {0 "5",
   7 "2",
   59 "m",
   58 "n",
   60 "e",
   1 "4",
   24 "7",
   55 "o",
   39 "j",
   54 "g",
   15 "1",
   48 "s",
   50 "r",
   31 "0",
   32 "h",
   40 "b",
   56 "i",
   33 "v",
   36 "l",
   41 "x",
   43 "y",
   61 "t",
   44 "z",
   28 "8",
   51 "w",
   34 "f",
   3 "3",
   57 "a",
   45 "q",
   53 "k",
   16 "6",
   38 "p",
   30 "9",
   52 "d",
   42 "c",
   49 "u"}

  ;; Then we finally create the lookup table

  (apply str (map #(indices->char % " ") (range 64)))
  "54 3   2       16       7   8 90hvf l pjbxcyzq  surwdkgoianmet  "

  ;; Spaces are used when there is no value at a given index.
  ;; we can even remove the last 2 spaces

  ;; In the end, morse-decode fn looks like:

  (defn morse-decode [code]
    (nth "54 3   2       16       7   8 90hvf l pjbxcyzq  surwdkgoianmet"
      (bit-and 63
        (reduce (fn [c b] (+ (bit-shift-left c 1) (case b \. 0 \- 1))) -2 code))))


  )
