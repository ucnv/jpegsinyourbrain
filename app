#!/usr/bin/env ruby
# encoding:utf-8

require 'pathname'
require 'docopt'
require File.expand_path '../lib/jpegs_in_your_brain.rb', $0

doc = <<-DOC.gsub '__FILE__', File.basename($0)
Jpegs In Your Brain

Usage:
  __FILE__ build <query> [-w <width>]
  __FILE__ corrupt <imagefile> <outfile>
  __FILE__ -h | --help

Options:
  -w <px>, --width <px>   Width of the html page. [default: 1280]
  -h --help               Show this screen.

Example:
  __FILE__ build portrait
  __FILE__ corrupt /input/image.jpg /output/image.jpg

DOC

begin
  opt = Docopt::docopt doc
  if opt['build']
    JpegsInYourBrain.main opt['<query>'], opt['--width']
  elsif opt['corrupt']
    JpegsInYourBrain.corrupt opt['<imagefile>'], opt['<outfile>']
  end
rescue Docopt::Exit => e
  puts e.message
end

