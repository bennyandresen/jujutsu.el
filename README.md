# jujutsu.el

An Emacs interface for [Jujutsu](https://github.com/martinvonz/jj) VCS, inspired by magit and humbly not attempting to match it in scope.

⚠️ **Pre-alpha - Experimental** ⚠️

![](https://raw.githubusercontent.com/bennyandresen/jujutsu.el/screenshots/Screenshot_20240827_164244.png)

## Overview

This package provides a UI for interacting with Jujutsu repositories in Emacs, built on top of a lightweight virtual DOM implementation. It consists of two main parts:

- `nx.el`: A minimal virtual DOM-like tree structure with diffing/patching, to be broken out upon stabilization
- `jujutsu-*.el`: The actual jujutsu interface built using nx

## Design Philosophy

The implementation takes inspiration from React and Clojure patterns:

- Data-driven UI architecture
- Immutable data structures 
- Efficient diffing/patching for updates
- Clear separation between state management and rendering

## Status

This is very much a work in progress and primarily serves as a personal experiment. The code reflects my background as a Clojurist, so expect some idiosyncrasies, when coming from a Emacs Lisp mindset.

While you're welcome to try it out or fork it, I'm not currently accepting pull requests or issues. The project needs significant work before it would be ready for MELPA or general use.

## Path to Alpha

Some key items that need attention before leaving pre-alpha:

- Stabilize the reconciler
  - Edge case handling in diff/patch operations
  - More robust node identity tracking
  - Better error reporting for invalid tree structures

- Design and implement global dispatcher system in nx
  - Need serious hammock time
  - Considering patterns from re-frame/replicant
  - Event-driven state management
  - Clear story for side effects

- Core porcelein functionality
  - Process buffer implementation (`$`)
    - Async command execution
    - Structured output parsing
    - Interactive error handling
  - Log viewer (`jj log` and `jj evolog`)
    - Handling logs for large histories
    - Advanced filtering/search
    - Summary buffer
  - Status buffer improvements
    - squash and/or edit workflow 

## Requirements

- Emacs 27.1+
- [Jujutsu](https://github.com/martinvonz/jj) VCS
- dash.el
- ht.el
- s.el

## License

AGPLv3
