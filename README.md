clj-toml - A TOML parser in Clojure
========

As the parser is only 30 LOC I'll just paste the entire source code here:

```clojure
(def re-keyval #"^\s*(\w+)\s*=?(.*)")
(def re-group #"^\s*\[([^\]]+)].*")
(defn clean-lines [coll]
  (drop-while #(re-find #"^\s*(#.*)?$" %) coll))
(defn partition-first [pred coll]
  (split-at (count (take-while pred coll)) coll))
(defn clean [s]
  (.trim (subs s 0 (if (neg? (.indexOf s "#")) (count s) (.indexOf s "#")))))
(defn parse-value [val]
  (let [val (clean val) c (str (first val))]		      
    (cond 
      (= c "\"") (subs val 1 (dec (count val)))
      (or (= c "[") (number? val)) (read-string val)
      (= val "true") true
      (= val "false") false
    :else val))) ;;Dates!
(defn parse [toml] 
  (loop [d {} lines (clojure.string/split-lines toml)]
    (let [line (first lines)]
      (if (nil? line)
        d ;;EOF
        (if-let [m (re-find re-group line)]
					(let [g (partition-first #(re-find re-keyval %) (clean-lines (drop 1 lines)))]
            (recur (assoc d (nth m 1) (parse (clojure.string/join "\n" (first g)))) 
              (second g))) ;;[group]
          (recur 
            (if-let [m (re-find re-keyval line)]
              (assoc d (nth m 1) (parse-value (nth m 2))) ;;key=value
              d) ;;ignore
            (drop 1 lines)))))))
```

Which when called with:

```clojure
(def toml-example "
# This is a TOML document. Boom.

title = \"TOML Example\"

[owner]
name = \"Tom Preston-Werner\"
organization = \"GitHub\"
bio = \"GitHub Cofounder & CEO\\nLikes tater tots and beer.\"
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = \"192.168.1.1\"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = \"10.0.0.1\"
  dc = \"eqdc10\"

  [servers.beta]
  ip = \"10.0.0.2\"
  dc = \"eqdc10\"

[clients]
data = [ [\"gamma\", \"delta\"], [1, 2] ] # just an update to make sure parsers support it  
")

(println (parse toml-example))
```

Will print out this native LISP data-structure:

    {clients {data [[gamma delta] [1 2]]}, servers.beta {dc eqdc10, ip 10.0.0.2}, servers.alpha {dc eqdc10, ip 10.0.0.1}, servers {}, 
    database {enabled true, connection_max 5000, ports [8001 8001 8002], server 192.168.1.1}, 
    owner {dob 1979-05-27T07:32:00Z, bio GitHub Cofounder & CEO\nLikes tater tots and beer., organization GitHub, name Tom Preston-Werner}, 
    title TOML Example}

### Enjoy!

