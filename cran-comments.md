── R CMD check results ──────────────────────────────────────────────────────────────────────── BeeBDC 1.3.2 ────
Duration: 5m 43.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


 - This updated version fixes issues with the beesTaxonomy and beesChecklist download issues, allows flexibilty with ggRichnessWrapper's file format, and provides improvements to dateFindR.
 - There are no major outstanding issues according to my tests
 - On the win test server I get the below error, however, that site loads fine on a browser so I hope it's a temporary rejection of the test server
 Found the following (possibly) invalid URLs:
  URL: http://www.gnu.org/licenses/gpl-3.0.html
    From: README.md
    Status: Error
    Message: Timeout was reached [www.gnu.org]:
      Failed to connect to www.gnu.org port 80 after 21021 ms: Could not connect to server


