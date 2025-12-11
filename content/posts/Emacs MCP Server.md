+++
title = "Emacs MCP Server"
author = ["Rolf Håvard Blindheim"]
description = "Emacs MCP Server is a package that implements a pure Elisp Model Context Protocol (MCP) server that lets your LLM of choice interact directly with your Emacs instance."
date = 2025-12-11T00:00:00+01:00
tags = ["emacs"]
categories = ["programming"]
draft = false
+++

When I first got my hands at a Claude Code subscription I was curious about what it could do and how it worked. I needed a project to practice on, so I decided to
see if I could get it to interact with Emacs. After a whole evening prompting and exploring Claude I managed to get a basic implementation working, and thus
[Emacs MCP Server](https://github.com/rhblind/emacs-mcp-server) was born.

I was quite satisfied with the result before I realized that there already existed packages that let's LLMs interact with Emacs.
Neverthless, I learned a whole lot about how to work with an LLM, the MCP server protocol and how to implement a server in Elisp.

As it turns out over the last months while I have been using it on my own, I have found several use-cases where having an LLM interact directly with Emacs is quite useful.
So I have continued to improve the package, and I now feel that it is in a state where it can be useful to others as well.

-   **Help me tune my Emacs config** - I often have small issues that's a bit annoying, but I don't really want to dig down to fix it. Now I can just ask Claude to fix it for me.
    It will look at my config, read the source code of packages I use and suggest fixes or improvements. It can evaluate the code directly in my Emacs instance,
    so I can see the results immediately.
-   **Claude Code IDE** - The [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el) package is brilliant, and has its own integration with Emacs, but I often find that claude suggests tool calls via the Emacs MCP Server
    instead of using its own built-in capabilities. This gives me a more seamless experience when working with Claude in Emacs with the best from both worlds.

If you want to try it out, you can find on [GitHub](https://github.com/rhblind/emacs-mcp-server) for now.
