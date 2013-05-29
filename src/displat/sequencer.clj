(ns displat.sequencer)

(def buf-size 16)
;; Sequencer adapted from Internal Sequencer example
(defonce buf-0 (buffer buf-size))
(defonce buf-1 (buffer buf-size))
(defonce buf-2 (buffer buf-size))
(defonce buf-3 (buffer buf-size))

(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count

(def BEAT-FRACTION "Number of global pulses per beat" 30)

;; Here we design synths that will drive our pulse buses.
(defsynth root-trg [rate 100]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

;; Here's a synth for playing back the samples with a bit of modulation
;; to keep things interesting.
(defsynth mono-sequencer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 sequencer 0 amp 1 pan 0]
  (let [cnt      (in:kr beat-cnt-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 sequencer cnt)
                      (= beat-num (mod cnt buf-size))
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out
     out-bus (* vol
                amp
                (pan2 (scaled-play-buf 1 buf rate bar-trg) pan)))))

;; Now, let's start up all the synths:
(do
  (def r-cnt (root-cnt))
  (def b-cnt (beat-cnt))
  (def b-trg (beat-trg))
  (def r-trg (root-trg))
