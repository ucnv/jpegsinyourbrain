require 'fileutils'
require 'tmpdir'
require 'open-uri'
require 'openssl'
require 'erb'
require 'watir-webdriver'
require 'watir-scroll'
require 'nokogiri'
require 'rmagick'
require File.expand_path '../jpeg_encoder.rb', __FILE__

class XJPG < JPGEncoder
  def process_DU *args
    @zigzag_o = @zigzag_o || @zigzag
    @zigzag = @zigzag_o.shuffle
    super *args
  end
end

module JpegsInYourBrain
  def self.main query, width
    distdir = Pathname.new 'dist'
    FileUtils.rm_r distdir if File.exists? distdir
    FileUtils.mkdir_p distdir.join('images')
    url = "https://www.google.com/search?q=%s&tbm=isch" % query
    browser =begin
      Watir::Browser.new :chrome
    rescue
      Watir::Browser.new :firefox
    end
    browser.window.resize_to(width, 600)
    browser.goto url
    browser.scroll.to :bottom
    browser.div(id: 'smc').wait_until_present
    browser.div(id: 'smc').input.click
    images = []
    divs = browser.divs(class: 'rg_di')
    divs.each.with_index do |div, idx|
      img = div.img
      unless img.html =~ / src="/
        browser.scroll.to div
        sleep 2
      end
      images << {
        width: div.wd.size.width,
        height: div.wd.size.height,
        src: img.html.sub(/[\s\S]* src="([^"]*)"[\s\S]*/, '\1')
      }
      print("\rScraping images: %4d / %d" % [idx + 1, divs.size])
    end
    browser.close
    print("\rScraping images: done.      ")
    print "\n"
    Dir.mktmpdir do |tmpdir|
      dir = Pathname.new tmpdir
      images.each_with_index do |img, idx|
        uri = img[:src]
        originalfile = dir.join('o.jpg')
        open(originalfile, 'w') do |o|
          d = ''
          if uri.start_with? 'http'
            open(uri, :ssl_verify_mode => OpenSSL::SSL::VERIFY_NONE) do |u|
              d = u.read
            end
          elsif uri.start_with? 'data'
            d = Base64.decode64 uri.sub('data:image/jpeg;base64,', '')
          end
          o.write d
        end
        resizedfile = dir.join('r.jpg')
        oimg = Magick::Image.read(originalfile).first
        oimg.resize!(img[:width], img[:height])
        oimg.write resizedfile
        self.corrupt(resizedfile, distdir.join('images/p%04d.jpg' % idx))
        print("\rEncoding images: %4d / %d" % [idx + 1, images.size])
      end
    end
    print("\rEncoding images: done.      ")
    print "\n"

    template = File.expand_path '../html.erb', __FILE__
    html = ERB.new(File.read(template)).result(binding)
    open(distdir.join('index.html'), 'w') do |f|
      f.write html
    end
  end

  def self.corrupt infile, outfile
    XJPG.new.encode(infile).output(outfile)
  end
end
