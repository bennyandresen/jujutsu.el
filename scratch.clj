(def diff-uneven [{:type :header, :content "@@ -56,15 +61,17 @@"}
                  {:type :context, :content "                             rev)))"}
                  {:type :context, :content "     (jj--run-command formatted)))"}
                  {:type :context, :content ""}
                  {:type :removed, :content "(defun jj--log-w/template (template)"}
                  {:type :removed,
                   :content "    \"Run `jj log' command with a custom TEMPLATE."}
                  {:type :added,
                   :content "(defun jj--log-w/template (template &optional revset)"}
                  {:type :added,
                   :content "  \"Run `jj log' command with a custom TEMPLATE and optional REVSET."}
                  {:type :context, :content ""}
                  {:type :context,
                   :content " TEMPLATE is a string containing the custom template for the `jj log' command."}
                  {:type :context, :content ""}
                  {:type :context,
                   :content " This function constructs and executes a `jj log' command with the given"}
                  {:type :context,
                   :content " template, disabling graph output and adding newlines between entries. It returns"}
                  {:type :context,
                   :content " the command's output as a string, with each log entry separated by newlines.\""}
                  {:type :removed,
                   :content "  (let* ((formatted (format \"log --no-graph --template \\\"%s ++ \\\\\\\"\\\\n\\\\n\\\\\\\"\\\"\""}
                  {:type :added,
                   :content "  (let* ((revset (or revset jujutsu-log-revset-fallback))"}
                  {:type :added,
                   :content "         (formatted (format \"log --revisions \\\"%s\\\" --no-graph --template \\\"%s ++ \\\\\\\"\\\\n\\\\n\\\\\\\"\\\"\""}
                  {:type :added, :content "                            revset"}
                  {:type :context, :content "                             template)))"}
                  {:type :context, :content "     (jj--run-command formatted)))"}])

(def diff-even [{:type :header, :content "@@ -75,7 +82,7 @@"}
                {:type :context, :content "        (s-split \"\\n\" it t)))"}
                {:type :context, :content ""}
                {:type :context, :content " (defun jj--map-to-escaped-string (map)"}
                {:type :removed,
                 :content "  \"Convert MAP (hash-table) to an escaped string.\""}
                {:type :added,
                 :content "  \"Convert MAP (hash-table) to an escaped string for use as a jj template.\""}
                {:type :context, :content "   (->> map"}
                {:type :context, :content "        (ht-map (lambda (key value)"}
                {:type :context,
                 :content "                  (format \"\\\\\\\"%s \\\\\\\" ++ %s ++ \\\\\\\"\\\\\\\\n\\\\\\\"\""}
                {:type :context, :content ""}])

(defn create-side-by-side-diff6
  "Creates a side-by-side diff representation from a sequence of diff lines.

  Takes a sequence of diff lines, where each line is a map with :type and :content keys.
  :type can be :header, :context, :removed, or :added.

  Returns a sequence of formatted strings, each representing a line in the side-by-side diff.
  The left side shows removed lines, the right side shows added lines, and context lines
  appear on both sides. Headers span the full width of the diff.

  The diff is aligned to accommodate the widest lines on each side, with a separator between.
  The entire input sequence represents a single diff chunk."
  [diff-git-chunk]
  (let [max-width (fn [type] (apply max (map #(count (:content %))
                                            (filter #(#{:context type} (:type %)) diff-git-chunk))))
        left-width (max-width :removed)
        right-width (max-width :added)
        pad (fn [s width] (format (str "%-" width "s") s))
        separator " | "
        format-line (fn [left right]
                      (str (pad (:content left) left-width)
                           separator
                           (pad (:content right) right-width)))
        format-header (fn [content]
                        (pad content (+ left-width right-width 3)))]
    (loop [line-map diff-git-chunk
           result []
           removed-lines []]
      (if (empty? line-map)
        (concat result (map #(format-line % {:content ""}) removed-lines))
        (let [{:keys [type content] :as chunk} (first line-map)]
          (case type
            :header (recur (rest line-map)
                           (conj result (format-header content))
                           removed-lines)
            :context (recur (rest line-map)
                            (conj result (format-line chunk chunk))
                            removed-lines)
            :removed (recur (rest line-map)
                            result
                            (conj removed-lines chunk))
            :added (if (empty? removed-lines)
                     (recur (rest line-map)
                            (conj result (format-line {:content ""} chunk))
                            removed-lines)
                     (recur (rest line-map)
                            (conj result (format-line (first removed-lines) chunk))
                            (rest removed-lines)))
            (recur (rest line-map) result removed-lines)))))))

(println "======\nUNEVEN\n======")
(doseq [line (create-side-by-side-diff6 diff-uneven)] (println line))

(println "======\n EVEN \n======")
(doseq [line (create-side-by-side-diff6 diff-even)] (println line))
