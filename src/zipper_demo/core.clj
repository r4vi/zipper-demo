(ns zipper-demo.core
  (:require
   [clojure.zip :as zip]
   [clojure.data.zip.xml :as zx]
   [clojure.data.xml :as xml]))

(def id-colors-table
  {1 "blue"
   2 "yellow"
   3 "pink"
   4 "white"
   5 "black"})

(def nested-vecs
  [:foo
   [:bar :baz [:bork]]])

(def z
  (zip/vector-zip nested-vecs))

(-> z
    zip/down
    zip/right
    zip/down
    zip/right
    zip/right
    zip/down
    (zip/edit #(clojure.string/upper-case %))
    zip/root)

(def pokemon
  (-> "pokemon.xml"
      clojure.java.io/resource
      clojure.java.io/input-stream
      xml/parse))

(def pokemon-zip (zip/xml-zip pokemon))



(def books
"
<catalog>
   <book id=\"bk101\">
      <author>Gambardella, Matthew</author>
      <title>XML Developer's Guide</title>
      <genre>Computer</genre>
      <publish_date>2000-10-01</publish_date>
      <description>An in-depth look at creating applications 
      with XML.</description>
   </book>
   <book id=\"bk102\">
      <author>Ralls, Kim</author>
      <title>Midnight Rain</title>
      <genre>Fantasy</genre>
      <publish_date>2000-12-16</publish_date>
      <description>A former architect battles corporate zombies, 
      an evil sorceress, and her own childhood to become queen 
      of the world.</description>
   </book>
   <book id=\"bk103\">
      <author>Corets, Eva</author>
      <title>Maeve Ascendant</title>
      <genre>Fantasy</genre>
      <publish_date>2000-11-17</publish_date>
      <description>After the collapse of a nanotechnology 
      society in England, the young survivors lay the 
      foundation for a new society.</description>
   </book>
   <book id=\"bk104\">
      <author>Corets, Eva</author>
      <title>Oberon's Legacy</title>
      <genre>Fantasy</genre>
      <publish_date>2001-03-10</publish_date>
      <description>In post-apocalypse England, the mysterious 
      agent known only as Oberon helps to create a new life 
      for the inhabitants of London. Sequel to Maeve 
      Ascendant.</description>
   </book>
   <book id=\"bk105\">
      <author>Corets, Eva</author>
      <title>The Sundered Grail</title>
      <genre>Fantasy</genre>
      <publish_date>2001-09-10</publish_date>
      <description>The two daughters of Maeve, half-sisters, 
      battle one another for control of England. Sequel to 
      Oberon's Legacy.</description>
   </book>
</catalog>
")

(def prices
  {:bk101 "44.95"
   :bk102 "12.50"
   :bk103 "24.20"
   :bk104 "4.20"
   :bk105 "45.00"})
(def books-as-elements (xml/parse-str books))
(def books-zipper (zip/xml-zip books-as-elements))

(defn editor [node]
  (let [id (-> node :attrs :id keyword)
        new-content (conj
                     (:content node)
                     (xml/element :price {} (get prices id "0.0")))]
    (assoc-in node [:content] new-content)))

(defn match-book? [loc]
  (let [tag (:tag (zip/node loc))]
    ;; true if tag is of type <path>
    (= :book tag)))


(defn tree-edit
  "Take a zipper, a function that matches a pattern in the tree,
   and a function that edits the current location in the tree.  Examine the tree
   nodes in depth-first order, determine whether the matcher matches, and if so
   apply the editor."
  [zipper matcher editor]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher loc)]
        (let [new-loc (zip/edit loc editor)]
          (if (not (= (zip/node new-loc) (zip/node loc)))
            (recur (zip/next new-loc))))
        (recur (zip/next loc))))))

(def edited (tree-edit books-zipper match-book? editor))
;;  print and write it.
(xml/indent-str
 edited)

;; <catalog>
;;<book id="bk101">
;;<price>44.95</price>
;;<author>Gambardella, Matthew</author>
;;<title>XML Developer's Guide</title>
;;<genre>Computer</genre>
;;<publish_date>2000-10-01</publish_date>
;;<description>
;;An in-depth look at creating applications with XML.
;;</description>
;;</book>

(with-open [f (clojure.java.io/writer  "/tmp/books_with_prices.xml")]
  (xml/emit edited f))
