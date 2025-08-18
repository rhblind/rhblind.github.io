# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Hugo-based static blog that primarily serves as a place to publish Emacs configuration and programming content. The blog is authored using Org Mode and published to GitHub Pages.

## Architecture

- **Content Source**: Blog posts are authored in `blog.org` using Org Mode with ox-hugo for export
- **Theme**: Uses the "hello-friend-ng" Hugo theme located in `themes/hello-friend-ng/`
- **Content Structure**: 
  - `content/posts/` contains exported markdown from Org Mode
  - `content/about.md` is the about page
  - `static/` contains images and other static assets
- **Deployment**: Automated via GitHub Actions workflow (`.github/workflows/pages.yml`)

## Common Commands

### Development
- `hugo server` - Start development server with live reload
- `hugo server -D` - Start server including draft content
- `hugo` - Build the site to `public/` directory
- `hugo --minify` - Build with minification (production build)

### Content Management
- Edit `blog.org` to add new posts or modify existing content
- Org Mode with `org-hugo-auto-export-mode` automatically exports to markdown
- Posts use ox-hugo properties for Hugo front matter configuration

### Site Configuration
- Main config: `config.toml`
- Theme customization: `layouts/` directory overrides
- Static assets: `static/` directory

## Org Mode Integration

The blog uses a unique workflow where content is authored in a single `blog.org` file:
- Each post is an Org heading with export properties
- Uses ox-hugo package for exporting to Hugo-compatible markdown
- Auto-export mode enables real-time conversion during editing

## Deployment

- Site deploys automatically on push to `main` branch
- GitHub Actions builds with Hugo 0.99.0 extended
- Publishes to GitHub Pages at https://rhblind.github.io/

## Theme Configuration

The hello-friend-ng theme is configured with:
- Dark/light theme toggle enabled
- Custom logo and branding
- Social media links (GitHub, LinkedIn, Email, Stack Overflow)
- Portrait image display
- Syntax highlighting with Pygments (monokai style)