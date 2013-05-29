(ns displat.junkspace
  (:use [overtone.live]))

(demo (sin-osc 400))

(def bowl-sample (load-sample "assets/bwbowl.wav"))
(def b (buffer 2048))

;; (
;;SynthDef
;;(loopy, {arg out = 0, buf = 0, rate = 1 ;
;;        var pos=Phasor.ar(buf, rate * BufRateScale.kr(buf), 0, BufFrames.kr(buf)) ;
;;          Out.ar(out, BufRd.ar(1, buf, pos)) ;
;;          }).send                            ;
;;)

(out 0 (buf-rd:ar 1 bowl-sample))



(defsynth buzz [speed 0.75 div 20]
  (let [dry  (play-buf 1 bowl-sample speed :loop 1)
        chain (fft (local-buf 2048 2) dry)
        chain (pv-brick-wall chain (sin-osc:kr [0.001 0.001]))
        out1  (ifft chain)]
     (out 0 out1)))

(buzz 30)
(stop)
(pulse-bowl)
(c-bus (control-bus))

(defsynth root-trig [rate 100]
  (out:kr c-bus (impulse:kr rate)))

(definst pingr [freq 400 div 20]
  (let [src   (sin-osc freq)
        t1    (pulse-divider:kr (in:kr c-bus) div)]
    (* (decay t1 0.1) src)))


(def r-trig (root-trig))

 (pulse-bowl 40)
(pingr)
(stop)
