── R CMD check results ───────────────────────────────────────────────────────────────────────────────────────────────────────── BeeBDC 1.3.5 ────
Duration: 7m 25.4s

❯ checking CRAN incoming feasibility ... [6s/145s] NOTE
  Maintainer: ‘James B. Dorey <jbdorey@me.com>’
  
  Found the following (possibly) invalid URLs:
    URL: https://www.discoverlife.org
      From: README.md
      Status: Error
      Message: libcurl error code 28:
        	Operation timed out after 60004 milliseconds with 0 bytes received

0 errors ✔ | 0 warnings ✔ | 1 note ✖

This update is to fix issues since 'taxadb' went to CRAN.
Please note, that I am aware of the potential timeout error from discoverlife. It is not always an issue, however, it might come up. I know that the site works, but I also know that it has been cracking down on AI crawlers that are otherwise chocking its servers. Hopefully it's not an issue.