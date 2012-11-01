====================
Prolog Web Framework
====================

An early developer version of the framework for developing applications in Prolog language.
The main goal of the framework is to allow every part of the program UI, logic and database access to be written in Prolog (or actually an object-oriented extension of the language).
The framework generates CSS, HTML and SQL from the declarative description of the application.

Status: Early alpha
The object oriented, more hierarchical extension of the language is partly implemented.

The logic to SQL bridge is not implemented, but pl2sql can be seen as an inspiration for the implementation:
https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/prolog/code/io/pl2sql/0.html

I'd like to extend pl2sql to be able to rewrite predicates that depend on each other to try to generate SQL when it can be done relatively easily.

Examples:
=========

Object oriented, hierarchical description of a page:
----------------------------------------------------

main::
  vbox @ (
    header @(background_color:blue),
	hbox @ (
	  vbox @ (
	    me.small,
		all_pages)),
      current_page)

		
widget:person.small::
  hbox @ (
    small_photo,
	name)
	
person:me::
 people[session.id]


An SQL for a counter of seen threads should be generated from this description, and CSS should be applied when it's rendered:
--------------------------------------------------------------------------

page.counter.render::
  background_color: 'light-blue'
  foreground_color: 'dark-blue'
 
pages_page.counter::
  comment_threads.count(has_new_comment)
  
has_new_comment::
  comments.has(new)

comment.new::
  not(seen)