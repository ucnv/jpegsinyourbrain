# This file is a kind of Ruby port of JPGEncoder.as
# <https://github.com/mikechambers/as3corelib/blob/master/src/com/adobe/images/JPGEncoder.as>

#  Copyright (c) 2008, Adobe Systems Incorporated
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without 
#  modification, are permitted provided that the following conditions are
#  met:
#
#  * Redistributions of source code must retain the above copyright notice, 
#    this list of conditions and the following disclaimer.
#  
#  * Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the 
#    documentation and/or other materials provided with the distribution.
#  
#  * Neither the name of Adobe Systems Incorporated nor the names of its 
#    contributors may be used to endorse or promote products derived from 
#    this software without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
#  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
#  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

require 'rmagick'

class JPGEncoder

  def initialize quality = 80
    init_values
    init_huffman_tbl
    init_category_number
    init_RGB_YUV_table
    self.quality quality
  end

  def init_values
    @byteout = []
    @bytenew = 0
    @bytepos = 7

    @YTable = Array.new 64
    @UVTable = Array.new 64
    @fdtbl_Y = Array.new 64
    @fdtbl_UV = Array.new 64
    @YDC_HT
    @UVDC_HT
    @YAC_HT
    @UVAC_HT

    @bitcode = Array.new 65535
    @category = Array.new 65535
    @output_fDCTQ = Array.new 64
    @DU = Array.new 64

    @YDU = Array.new 64
    @UDU = Array.new 64
    @VDU = Array.new 64
    @RGB_YUV_TABLE = Array.new 2048

    @zigzag = [
      0, 1, 5, 6,14,15,27,28,
      2, 4, 7,13,16,26,29,42,
      3, 8,12,17,25,30,41,43,
      9,11,18,24,31,40,44,53,
      10,19,23,32,39,45,52,54,
      20,22,33,38,46,51,55,60,
      21,34,37,47,50,56,59,61,
      35,36,48,49,57,58,62,63
    ]

    @std_dc_luminance_nrcodes = [0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0]
    @std_dc_luminance_values = [0,1,2,3,4,5,6,7,8,9,10,11]
    @std_ac_luminance_nrcodes = [0,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,0x7d]
    @std_ac_luminance_values = [
      0x01,0x02,0x03,0x00,0x04,0x11,0x05,0x12,
      0x21,0x31,0x41,0x06,0x13,0x51,0x61,0x07,
      0x22,0x71,0x14,0x32,0x81,0x91,0xa1,0x08,
      0x23,0x42,0xb1,0xc1,0x15,0x52,0xd1,0xf0,
      0x24,0x33,0x62,0x72,0x82,0x09,0x0a,0x16,
      0x17,0x18,0x19,0x1a,0x25,0x26,0x27,0x28,
      0x29,0x2a,0x34,0x35,0x36,0x37,0x38,0x39,
      0x3a,0x43,0x44,0x45,0x46,0x47,0x48,0x49,
      0x4a,0x53,0x54,0x55,0x56,0x57,0x58,0x59,
      0x5a,0x63,0x64,0x65,0x66,0x67,0x68,0x69,
      0x6a,0x73,0x74,0x75,0x76,0x77,0x78,0x79,
      0x7a,0x83,0x84,0x85,0x86,0x87,0x88,0x89,
      0x8a,0x92,0x93,0x94,0x95,0x96,0x97,0x98,
      0x99,0x9a,0xa2,0xa3,0xa4,0xa5,0xa6,0xa7,
      0xa8,0xa9,0xaa,0xb2,0xb3,0xb4,0xb5,0xb6,
      0xb7,0xb8,0xb9,0xba,0xc2,0xc3,0xc4,0xc5,
      0xc6,0xc7,0xc8,0xc9,0xca,0xd2,0xd3,0xd4,
      0xd5,0xd6,0xd7,0xd8,0xd9,0xda,0xe1,0xe2,
      0xe3,0xe4,0xe5,0xe6,0xe7,0xe8,0xe9,0xea,
      0xf1,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7,0xf8,
      0xf9,0xfa
    ]

    @std_dc_chrominance_nrcodes = [0,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0]
    @std_dc_chrominance_values = [0,1,2,3,4,5,6,7,8,9,10,11]
    @std_ac_chrominance_nrcodes = [0,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,0x77]
    @std_ac_chrominance_values = [
      0x00,0x01,0x02,0x03,0x11,0x04,0x05,0x21,
      0x31,0x06,0x12,0x41,0x51,0x07,0x61,0x71,
      0x13,0x22,0x32,0x81,0x08,0x14,0x42,0x91,
      0xa1,0xb1,0xc1,0x09,0x23,0x33,0x52,0xf0,
      0x15,0x62,0x72,0xd1,0x0a,0x16,0x24,0x34,
      0xe1,0x25,0xf1,0x17,0x18,0x19,0x1a,0x26,
      0x27,0x28,0x29,0x2a,0x35,0x36,0x37,0x38,
      0x39,0x3a,0x43,0x44,0x45,0x46,0x47,0x48,
      0x49,0x4a,0x53,0x54,0x55,0x56,0x57,0x58,
      0x59,0x5a,0x63,0x64,0x65,0x66,0x67,0x68,
      0x69,0x6a,0x73,0x74,0x75,0x76,0x77,0x78,
      0x79,0x7a,0x82,0x83,0x84,0x85,0x86,0x87,
      0x88,0x89,0x8a,0x92,0x93,0x94,0x95,0x96,
      0x97,0x98,0x99,0x9a,0xa2,0xa3,0xa4,0xa5,
      0xa6,0xa7,0xa8,0xa9,0xaa,0xb2,0xb3,0xb4,
      0xb5,0xb6,0xb7,0xb8,0xb9,0xba,0xc2,0xc3,
      0xc4,0xc5,0xc6,0xc7,0xc8,0xc9,0xca,0xd2,
      0xd3,0xd4,0xd5,0xd6,0xd7,0xd8,0xd9,0xda,
      0xe2,0xe3,0xe4,0xe5,0xe6,0xe7,0xe8,0xe9,
      0xea,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7,0xf8,
      0xf9,0xfa
    ]
  end

  def quality value = nil
    return @current_quality if value.nil?
    value = 1 if value <= 0
    value = 100 if value > 100
    return if @current_quality == value
    sf = 0
    if value < 50
      sf = (5000 / value).floor
    else
      sf = (200 - value * 2).floor
    end
    init_quant_tables sf
    @current_quality = value
  end

  def encode filename
    img = Magick::ImageList.new(filename).first
    width = img.columns
    height = img.rows
    image_data = img.get_pixels 0, 0, width, height

    @byteout = []
    @bytenew = 0
    @bytepos = 7

    # header
    write_word 0xFFD8  # SOI
    write_APP0
    write_DQT
    write_SOF0 width, height
    write_DHT
    write_SOS

    vDCY = 0
    vDCU = 0
    vDCV = 0

    @bytenew = 0
    @bytepos = 7
    quad_width = width * 4
    triple_width = width * 3

    x, y = 0, 0
    r, b, g = ()
    start, p, col, row, pos = ()
    t = @RGB_YUV_TABLE

    while y < height do
      x = 0
      while x < quad_width do
        start = quad_width * y + x - 1
        p = start
        col = -1
        row = 0
        64.times do |pos|
          row = pos >> 3 # /8
          col = (pos & 7) * 4 # %8
          p = start + (row * quad_width) + col
          if y + row >= height      # padding bottom
            p -= quad_width * (y + 1 + row - height)
          end
          if x + col > quad_width   # padding right
            p -= (x + col) - quad_width + 4
          end

          pixel = image_data[p / 4]
          p += 3
          r = (pixel.red / 256).to_i
          g = (pixel.green / 256).to_i
          b = (pixel.blue / 256).to_i

          @YDU[pos] = ((t[r]               + t[(g +  256) >> 0] + t[(b +  512)>>0]) >> 16) - 128
          @UDU[pos] = ((t[(r +  768) >> 0] + t[(g + 1024) >> 0] + t[(b + 1280)>>0]) >> 16) - 128
          @VDU[pos] = ((t[(r + 1280) >> 0] + t[(g + 1536) >> 0] + t[(b + 1792)>>0]) >> 16) - 128
        end
        vDCY = process_DU @YDU, @fdtbl_Y, vDCY, @YDC_HT, @YAC_HT
        vDCU = process_DU @UDU, @fdtbl_UV, vDCU, @UVDC_HT, @UVAC_HT
        vDCV = process_DU @VDU, @fdtbl_UV, vDCV, @UVDC_HT, @UVAC_HT
        x += 32
      end
      y += 8
    end

    # byte align
    if @bytepos >= 0
      fillbits = []
      fillbits[1] = @bytepos + 1
      fillbits[0] = (1 << (@bytepos + 1)) - 1
      write_bits fillbits
    end

    write_word 0xFFD9 # EOI
    self
  end

  def output filename
    File.open(filename, 'w') do |f|
      x = @byteout.join
      f.print x
    end
    @byteout = []
  end

  def process_DU cdu, fdtbl, dc, htdc, htac
    eob = htac[0x00]
    m16zeroes = htac[0xF0]
    pos = 0
    vDU_DCT = f_DCT_quant(cdu, fdtbl)
    64.times do |i|
      @DU[@zigzag[i]] = vDU_DCT[i]
    end
    diff = @DU[0] - dc
    dc = @DU[0]
    if diff == 0
      write_bits htdc[0]
    else
      pos = 32767 + diff
      write_bits htdc[@category[pos]]
      write_bits @bitcode[pos]
    end
    end0pos = 63
    while end0pos > 0 && @DU[end0pos] == 0 do
      end0pos -= 1
    end
    if end0pos == 0
      write_bits eob
      return dc
    end
    i = 1
    lng = 0
    while i <= end0pos do
      startpos = i
      while (@DU[i] == 0) && (i <= end0pos) do
        i += 1
      end
      nrzeroes = i - startpos
      if nrzeroes >= 16
        lng = nrzeroes >> 4
        lng.times do
          write_bits m16zeroes
        end
        nrzeroes = nrzeroes & 0xF
      end
      pos = 32767 + @DU[i]
      write_bits htac[(nrzeroes << 4) + @category[pos]]
      write_bits @bitcode[pos]
      i += 1
    end
    if end0pos != 63
      write_bits eob
    end
    dc
  end

  def init_quant_tables sf
    yqt = [
      16, 11, 10, 16, 24, 40, 51, 61,
      12, 12, 14, 19, 26, 58, 60, 55,
      14, 13, 16, 24, 40, 57, 69, 56,
      14, 17, 22, 29, 51, 87, 80, 62,
      18, 22, 37, 56, 68,109,103, 77,
      24, 35, 55, 64, 81,104,113, 92,
      49, 64, 78, 87,103,121,120,101,
      72, 92, 95, 98,112,100,103, 99
    ]
    64.times do |i|
      t = ((yqt[i] * sf + 50) / 100).floor
      t = 1 if t < 1
      t = 255 if t > 255
      @YTable[@zigzag[i]] = t
    end
    uvqt = [
      17, 18, 24, 47, 99, 99, 99, 99,
      18, 21, 26, 66, 99, 99, 99, 99,
      24, 26, 56, 99, 99, 99, 99, 99,
      47, 66, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99, 99, 99, 99
    ]
    64.times do |i|
      u = ((uvqt[i] * sf + 50) / 100).floor
      u = 1 if u < 1
      u = 255 if u > 255
      @UVTable[@zigzag[i]] = u
    end
    aasf = [
      1.0, 1.387039845, 1.306562965, 1.175875602,
      1.0, 0.785694958, 0.541196100, 0.275899379
    ]
    k = 0
    8.times do |row|
      8.times do |col|
        @fdtbl_Y[k]  = (1.0 / (@YTable [@zigzag[k]] * aasf[row] * aasf[col] * 8.0))
        @fdtbl_UV[k] = (1.0 / (@UVTable[@zigzag[k]] * aasf[row] * aasf[col] * 8.0))
        k += 1
      end
    end
  end

  def init_huffman_tbl
    @YDC_HT = compute_huffman_tbl @std_dc_luminance_nrcodes, @std_dc_luminance_values
    @UVDC_HT = compute_huffman_tbl @std_dc_chrominance_nrcodes, @std_dc_chrominance_values
    @YAC_HT = compute_huffman_tbl @std_ac_luminance_nrcodes, @std_ac_luminance_values
    @UVAC_HT = compute_huffman_tbl @std_ac_chrominance_nrcodes, @std_ac_chrominance_values
  end

  def compute_huffman_tbl nrcodes, std_table
    codevalue = 0
    pos_in_table = 0
    ht = []
    16.times do |i|
      k = i + 1
      nrcodes[k].times do |j|
        ht[std_table[pos_in_table]] = []
        ht[std_table[pos_in_table]][0] = codevalue
        ht[std_table[pos_in_table]][1] = k
        pos_in_table += 1
        codevalue += 1
      end
      codevalue *= 2
    end
    ht
  end

  def init_category_number
    nrlower = 1
    nrupper = 2
    15.times do |i|
      cat = i + 1
      # Positive numbers
      (nrlower..(nrupper - 1)).each do |nr|
        @category[32767 + nr] = cat
        @bitcode[32767 + nr] = []
        @bitcode[32767 + nr][1] = cat
        @bitcode[32767 + nr][0] = nr
      end
      # Negative numbers
      ((-(nrupper-1))..(-nrlower)).each do |nrneg|
        @category[32767 + nrneg] = cat
        @bitcode[32767 + nrneg] = []
        @bitcode[32767 + nrneg][1] = cat
        @bitcode[32767 + nrneg][0] = nrupper - 1 + nrneg
      end
      nrlower = nrlower << 1
      nrupper = nrupper << 1
    end
  end

  def init_RGB_YUV_table
    256.times do |i|
      @RGB_YUV_TABLE[i]           =  19595 * i
      @RGB_YUV_TABLE[(i+ 256)>>0] =  38470 * i
      @RGB_YUV_TABLE[(i+ 512)>>0] =   7471 * i + 0x8000
      @RGB_YUV_TABLE[(i+ 768)>>0] = -11059 * i
      @RGB_YUV_TABLE[(i+1024)>>0] = -21709 * i
      @RGB_YUV_TABLE[(i+1280)>>0] =  32768 * i + 0x807FFF
      @RGB_YUV_TABLE[(i+1536)>>0] = -27439 * i
      @RGB_YUV_TABLE[(i+1792)>>0] = - 5329 * i
    end
  end

  def f_DCT_quant data, fdtbl
    d0, d1, d2, d3, d4, d5, d6, d7 = ()
    off = 0

    8.times do |i|
      d0 = data[off]
      d1 = data[off + 1]
      d2 = data[off + 2]
      d3 = data[off + 3]
      d4 = data[off + 4]
      d5 = data[off + 5]
      d6 = data[off + 6]
      d7 = data[off + 7]

      tmp0 = d0 + d7
      tmp7 = d0 - d7
      tmp1 = d1 + d6
      tmp6 = d1 - d6
      tmp2 = d2 + d5
      tmp5 = d2 - d5
      tmp3 = d3 + d4
      tmp4 = d3 - d4

      tmp10 = tmp0 + tmp3
      tmp13 = tmp0 - tmp3
      tmp11 = tmp1 + tmp2
      tmp12 = tmp1 - tmp2

      data[off] = tmp10 + tmp11
      data[off + 4] = tmp10 - tmp11

      z1 = (tmp12 + tmp13) * 0.707106781
      data[off + 2] = tmp13 + z1
      data[off + 6] = tmp13 - z1

      tmp10 = tmp4 + tmp5
      tmp11 = tmp5 + tmp6
      tmp12 = tmp6 + tmp7

      z5 = (tmp10 - tmp12) * 0.382683433
      z2 = 0.541196100 * tmp10 + z5
      z4 = 1.306562965 * tmp12 + z5
      z3 = tmp11 * 0.707106781

      z11 = tmp7 + z3
      z13 = tmp7 - z3

      data[off + 5] = z13 + z2
      data[off + 3] = z13 - z2
      data[off + 1] = z11 + z4
      data[off + 7] = z11 - z4

      off += 8
    end

    off = 0
    8.times do |i|
      d0 = data[off]
      d1 = data[off + 8]
      d2 = data[off + 16]
      d3 = data[off + 24]
      d4 = data[off + 32]
      d5 = data[off + 40]
      d6 = data[off + 48]
      d7 = data[off + 56]

      tmp0p2 = d0 + d7
      tmp7p2 = d0 - d7
      tmp1p2 = d1 + d6
      tmp6p2 = d1 - d6
      tmp2p2 = d2 + d5
      tmp5p2 = d2 - d5
      tmp3p2 = d3 + d4
      tmp4p2 = d3 - d4

      tmp10p2 = tmp0p2 + tmp3p2
      tmp13p2 = tmp0p2 - tmp3p2
      tmp11p2 = tmp1p2 + tmp2p2
      tmp12p2 = tmp1p2 - tmp2p2

      data[off] = tmp10p2 + tmp11p2
      data[off + 32] = tmp10p2 - tmp11p2

      z1p2 = (tmp12p2 + tmp13p2) * 0.707106781
      data[off + 16] = tmp13p2 + z1p2
      data[off + 48] = tmp13p2 - z1p2

      tmp10p2 = tmp4p2 + tmp5p2
      tmp11p2 = tmp5p2 + tmp6p2
      tmp12p2 = tmp6p2 + tmp7p2

      z5p2 = (tmp10p2 - tmp12p2) * 0.382683433
      z2p2 = 0.541196100 * tmp10p2 + z5p2
      z4p2 = 1.306562965 * tmp12p2 + z5p2
      z3p2 = tmp11p2 * 0.707106781

      z11p2 = tmp7p2 + z3p2
      z13p2 = tmp7p2 - z3p2

      data[off + 40] = z13p2 + z2p2
      data[off + 24] = z13p2 - z2p2
      data[off +  8] = z11p2 + z4p2
      data[off + 56] = z11p2 - z4p2

      off += 1
    end

    fDCTQ = nil
    64.times do |i|
      fDCTQ = data[i].to_f * fdtbl[i]
      @output_fDCTQ[i] = (fDCTQ > 0.0) ? (fDCTQ + 0.5).floor : (fDCTQ - 0.5).ceil
    end
    @output_fDCTQ
  end

  def write_APP0
    write_word 0xFFE0 # marker
    write_word 16     # length
    write_byte 0x4A   # J
    write_byte 0x46   # F
    write_byte 0x49   # I
    write_byte 0x46   # F
    write_byte 0      # = "JFIF",'\0'
    write_byte 1      # versionhi
    write_byte 1      # versionlo
    write_byte 0      # xyunits
    write_word 1      # xdensity
    write_word 1      # ydensity
    write_byte 0      # thumbnwidth
    write_byte 0      # thumbnheight
  end

  def write_DQT
    write_word 0xFFDB # marker
    write_word 132    # length
    write_byte 0
    64.times do |i|
      write_byte @YTable[i]
    end
    write_byte 1
    64.times do |i|
      write_byte @UVTable[i]
    end
  end

  def write_SOF0 width, height
    write_word 0xFFC0 # marker
    write_word 17     # length, truecolor YUV JPG
    write_byte 8      # precision
    write_word height
    write_word width
    write_byte 3      # nrofcomponents
    write_byte 1      # IdY
    write_byte 0x11   # HVY
    write_byte 0      # QTY
    write_byte 2      # IdU
    write_byte 0x11   # HVU
    write_byte 1      # QTU
    write_byte 3      # IdV
    write_byte 0x11   # HVV
    write_byte 1      # QTV
  end

  def write_DHT
    write_word 0xFFC4 # marker
    write_word 0x01A2 # length

    write_byte 0      # HTYDCinfo
    16.times do |i|
      write_byte @std_dc_luminance_nrcodes[i+1]
    end
    12.times do |i|
      write_byte @std_dc_luminance_values[i]
    end

    write_byte 0x10  # HTYACinfo
    16.times do |i|
      write_byte @std_ac_luminance_nrcodes[i+1]
    end
    162.times do |i|
      write_byte @std_ac_luminance_values[i]
    end

    write_byte 1     # HTUDCinfo
    16.times do |i|
      write_byte @std_dc_chrominance_nrcodes[i+1]
    end
    12.times do |i|
      write_byte @std_dc_chrominance_values[i]
    end

    write_byte 0x11   # HTUACinfo
    16.times do |i|
      write_byte @std_ac_chrominance_nrcodes[i+1]
    end
    162.times do |i|
      write_byte @std_ac_chrominance_values[i]
    end
  end

  def write_SOS
    write_word 0xFFDA # marker
    write_word 12     # length
    write_byte 3      # nrofcomponents
    write_byte 1      # IdY
    write_byte 0      # HTY
    write_byte 2      # IdU
    write_byte 0x11   # HTU
    write_byte 3      # IdV
    write_byte 0x11   # HTV
    write_byte 0      # Ss
    write_byte 0x3f   # Se
    write_byte 0      # Bf
  end

  def write_bits bs
    value = bs[0]
    posval = bs[1] - 1
    while posval >= 0 do
      if (value & (1 << posval)) != 0
        @bytenew = @bytenew | (1 << @bytepos)
      end
      posval -= 1
      @bytepos -= 1
      if @bytepos < 0
        if @bytenew == 0xFF
          write_byte 0xFF
          write_byte 0
        else
          write_byte @bytenew
        end
        @bytepos = 7
        @bytenew = 0
      end
    end
  end

  def write_byte value
    @byteout.push [value].pack('C*')
  end

  def write_word value
    write_byte((value >> 8) & 0xFF)
    write_byte( value       & 0xFF)
  end

end
