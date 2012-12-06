#! /usr/bin/ruby
File.open("input", "w") { |file|
	$stdin.each_line { |line|
		file.puts line
	}
}