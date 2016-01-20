(ns blog-posts.core
  (:require
     [clojure.edn :as edn]
     [clojure.pprint :as pp]
     [clj-uuid :as uuid]
     [clj-time.core :as t]
     [clj-time.coerce :as tc])
  (:import java.io.File))

;;; ===========================
;;; Setup
;;; ===========================

(def user :ecallen)

(def blog-file "ecallen.posts")

(defn retrieve [file]
  (ref (edn/read-string (slurp file))))

(defn save [data file append-mode]
  (with-open [f (clojure.java.io/writer file :append append-mode)]
    (pp/pprint @data f)))

(defn posts-db [fname]
  (if (not (.exists (File. fname)))
    (save (ref {}) fname true))
  (retrieve fname))

(defn load-posts [fname] (posts-db fname))

(defn add-post [title author fname posts]
  (let [id (-> (uuid/v4) str keyword)
        mp {:title title
            :author author
            :post-timestamp (tc/to-long (t/now))
            :last-update (tc/to-long (t/now))
            :revision 0
            :text (slurp fname)}]
    (dosync (alter posts assoc-in [user id] mp))))

(defn idx->id [idx posts]
 (nth (vec (sort (keys (user @posts)))) idx))

(defn list-posts [posts]
  (let [ids (sort (keys (user @posts)))]
   (for [id ids]
    (prn (.indexOf ids id) (get-in @posts [user id :title]) id))))

(defn update-post-text [idx fname posts]
  (let [id (idx->id idx)
        text (slurp fname)]
      (dosync (alter posts assoc-in [user id :text] text))))

; (nth (vec (sort (keys (user @posts)))) 1)
