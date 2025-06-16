── R CMD check results ─────────────────────────────────────────────────────────────────────────────── BeeBDC 1.3.0 ────
Duration: 6m 59.3s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

❯ checking examples ... [36s/37s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                      user system elapsed
  jbd_Ctrans_chunker 4.623  0.335   5.015

0 errors ✔ | 0 warnings ✔ | 2 notes ✖


 - There may be test issues with ggplot2 and cowplot in advance of ggplot2's update. However, I am pushing fixes to those updates in advance.
 — I hope you don't mind the +0.015 second time on testing jbd_Ctrans_chunker

