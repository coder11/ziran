(ns ziran.util
  (:import [java.io PushbackReader]
           [java.util Stack]
           [java.lang StringBuilder]))

(defprotocol TokenReader
  (read-ch [this])
  (unread-ch [this count])
  (collect-token [this])
  (reset-token [this]))

(defn token-reader [r]
  (let [r          (if (instance? PushbackReader r)
                     r
                     (PushbackReader. r 128))
        pos        (volatile! 0)
        col        (volatile! 0)
        line       (volatile! 0)
        do-mark    (fn [] {:pos @pos
                           :col @col
                           :line @line})
        mark       (volatile! (do-mark))
        buf        (volatile! (StringBuilder.))
        prev-lines (Stack.)]

    (reify TokenReader
      (read-ch [this]
        (let [ch (.read r)]
          (if (= ch -1)
            nil
            (let [ch (char ch)]
              (if (= ch \newline)
                (do (.push prev-lines @col)
                    (vreset! col 0)
                    (vswap! line inc))
                (vswap! col inc))
              (vswap! pos inc)
              (.append @buf ch)
              ch))))

      (unread-ch [this count]
        (loop [ix (dec (.length @buf))
               c  count]
          (when (and (> c 0)
                     (>= ix 0))
            (if (= @col 0)
              (do
                (vreset! col (.pop prev-lines))
                (vswap! line dec))
              (vswap! col dec))
            (.unread r (int (.charAt @buf ix)))
            (.deleteCharAt @buf ix)
            (vswap! pos dec)
            (recur (dec ix) (dec c)))))

      (collect-token [this]
        (let [res {:string     (.toString @buf)
                   :len        (.length @buf)
                   :pos-start  (:pos @mark)
                   :pos-end    @pos
                   :line-start (:line @mark)
                   :line-end   @line
                   :col-start  (:col @mark)
                   :col-end    @col}]
          (vreset! mark (do-mark))
          (vreset! buf (StringBuilder.))
          (.clear prev-lines)
          res))

      (reset-token [this]
        (unread-ch this Integer/MAX_VALUE)
        (vreset! mark (do-mark))))))