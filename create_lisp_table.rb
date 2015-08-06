require 'rubygems'
require 'open-uri'
require 'csv'

CSV_URL = 'https://www.iana.org/assignments/character-sets/character-sets-1.csv'
LISP_FILE = 'src/official-encoding-aliases.lisp'

File.open(LISP_FILE, 'w') do |out|
  out.print <<EOS
;; Generate from Official Encoding name list.
;; See:
;; https://www.iana.org/assignments/character-sets/character-sets.xhtml
(in-package #:babel-encodings)

(defvar *official-encoding-aliases*
  `(
EOS

  CSV.foreach(open(CSV_URL), headers: true) do |row|
    basename = row[1].chomp
    r5 = row[5]
    if r5
      aliases = [basename]
      r5.each_line do |line|
        line.strip!
        aliases << line
      end
      out.puts "    (#{aliases.collect { |a| ("\"" + a + "\"") }.join(' ')})"
    end
  end

  out.puts '))'
end
