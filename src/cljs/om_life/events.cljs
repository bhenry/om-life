(ns om-life.events
  (:require [cljs.core.async :refer [chan <! close!] :as a])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defonce app-state (atom {}))

(defonce subscribers (atom {}))
(defonce publisher (chan 2048))
(defonce publication (a/pub publisher :type))

(defrecord Event [type message])

(defn event
  ([type]
     (event type nil))
  ([type message]
     (->Event
      type
      message)))

(defn publish! [^Event e]
  {:pre [(instance? Event e)]}
  (a/put! publisher e))

(defn unsubscribe! [chan-key]
  (when-let [{:keys [chan topics]} (get @subscribers chan-key)]
    (doseq [topic topics]
      (a/unsub publication topic chan))
    (a/close! chan)
    (swap! subscribers dissoc chan-key)))

(defn subscribe! [event-types chan-key ch]
  (doseq [event-type event-types]
    (a/sub publication event-type ch true))
  (swap! subscribers assoc chan-key {:chan ch, :topics event-types}))

(defn do-each-in-thread [f c]
  (go
   (loop []
     (let [v (<! c)]
       (when-not (nil? v)
         (f (:message v))
         (recur))))))

(defn subscriber [buf-or-n event-types body-fn]
  (let [ch (chan buf-or-n)
        chan-key body-fn]
    (unsubscribe! chan-key)
    (subscribe! event-types chan-key ch)
    (try
      (do-each-in-thread body-fn ch)
      (catch js/Error e
        (unsubscribe! chan-key)
        (js/console.warn
         (ex-info
          "exception thrown while executing subscriber body"
          {:subscriber chan-key
           :buf-or-n buf-or-n
           :event-types event-types}
          e))))))

(defn bind
  [event-kws handler-fn]
  {:pre [(not (nil? handler-fn))]}
  (subscriber (a/sliding-buffer 10)
              event-kws
              handler-fn))

(defn subscriptions [& pairs]
  (doseq [[evts handler] (partition 2 pairs)]
            (bind evts handler)))
