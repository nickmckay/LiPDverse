#!/usr/bin/python
from bs4 import BeautifulSoup, Tag, NavigableString
import codecs
import os
import sys

# Get all html files from the html directory. Loop for each
files = [file for file in os.listdir("html") if file.endswith(".html")]
for file in files:
	try:
		# Open the html and create BS object
		fp = os.path.join("html", file)
		f = codecs.open(fp, 'r', 'utf-8')
		soup = BeautifulSoup(f.read(), "html.parser")
		# Insert the tracking code at the end of the body, befor the close body tag
		extraSoup = BeautifulSoup("""<script type="text/javascript">
		var sc_project=11849428; 
		var sc_invisible=1; 
		var sc_security="6b896598"; 
		var sc_https=1; 
		</script>
		<script type="text/javascript"
		src="https://www.statcounter.com/counter/counter.js"
		async></script>
		<noscript><div class="statcounter"><a title="Web Analytics
		Made Easy - StatCounter" href="http://statcounter.com/"
		target="_blank"><img class="statcounter"
		src="//c.statcounter.com/11849428/0/6b896598/1/" alt="Web
		Analytics Made Easy - StatCounter"></a></div></noscript>""", "html.parser")
		soup.body.insert(len(soup.body.contents), extraSoup)
		# Open a new html file (overwriting the original) with the new html code. 
		with open(fp, 'w+') as wr:
			wr.write(soup.encode('utf-8').decode('ascii', 'ignore'))
		# Note when a file is written, so we know it worked.
		print(fp)
	except Exception as e:
		# Something went wrong. Oops
		print("{}: {}".format(file, e))
