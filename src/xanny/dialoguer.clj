(ns xanny.dialoguer)

;give multiple scenes of dialogue.
(def dialogue {:character {:intro "Greatings" :responses {}}})

(defn conversation []
  (let [input (read)
        color (cond (= input 1) :B
                    (= input 2) :Y
                    (= input 3) :R
                    (= input 4) :G)]
    (println color)))

