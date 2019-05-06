require 'gradesheet'
require 'write_xlsx'
require 'stringio'
require 'audit'

class Spreadsheet
  protected
  
  def self.col_index(name)
    @columns.find_index{|c| c.name == name}
  end
  def self.col_delta(from, to)
    col_index(from) - col_index(to)
  end
  def self.row_index(col_name, value)
    ci = col_index(col_name)
    @rows.find_index{|r| r[ci] == value} + @header_rows.length
  end
  def self.col_name(index)
    # zero-based, so col_name(0) == "A", col_name(26) == "AA"
    alph = ("A".."Z").to_a
    s, q = "", index + 1
    (q, r = (q - 1).divmod(26)) && s.prepend(alph[r]) until q.zero?
    s
  end

  public
  attr_reader :sheets

  class Col
    attr_reader :name
    attr_reader :type
    attr_accessor :col
    def initialize(name, type=nil)
      @name = name
      @type = type || "String"
    end
  end

  class Formula
    attr_accessor :function
    attr_accessor :args
    attr_accessor :value
    def initialize(value, function, *args)
      @value = value
      @function = function
      @args = args
    end
    def to_s
      if (@function.length == 1) && (@args.length == 2)
        "(#{@args[0]} #{@function} #{@args[1]})"
      else
        "#{@function}(#{@args.map(&:to_s).join(',')})"
      end
    end
  end

  class CellRef
    attr_accessor :sheet_name
    attr_accessor :value
    attr_accessor :col
    attr_accessor :row
    attr_accessor :col_abs
    attr_accessor :row_abs
    def initialize(sheet_name, col, col_abs, row, row_abs, value)
      @sheet_name = sheet_name
      @col = col
      @row = row
      @col_abs = "$" if col_abs
      @row_abs = "$" if row_abs
    end
    def to_s
      ans = "#{@col_abs}#{@col}#{@row_abs}#{@row}"
      unless @sheet_name.nil?
        ans = "#{@sheet_name}!#{ans}"
      end
      ans
    end
  end

  class Chart
    attr_accessor :series
    def initialize
      @series = []
    end
    def insert_into(workbook, ws, cellref)
      chart = workbook.add_chart(type: 'column', embedded: 1)
      @series.each do |s|
        chart.add_series(name: s.name.to_s,
                         categories: "=#{ws.name}!#{s.categories}", values: "=#{ws.name}!#{s.values}")
      end
      chart.set_legend(position: 'none')
      ws.insert_chart(cellref.to_s, chart)
    end
    def add_series(name, categories, values)
      @series << ChartSeries.new(name, categories, values)
    end
  end

  class ChartSeries
    attr_accessor :name
    attr_accessor :categories
    attr_accessor :values
    def initialize(name, categories, values)
      @name = name
      @categories = categories
      @values = values
    end
  end

  class Range
    attr_accessor :from_col
    attr_accessor :from_row
    attr_accessor :to_col
    attr_accessor :to_row
    attr_accessor :from_col_abs
    attr_accessor :from_row_abs
    attr_accessor :to_col_abs
    attr_accessor :to_row_abs
    def initialize(from_col, from_col_abs, from_row, from_row_abs, to_col, to_col_abs, to_row, to_row_abs)
      @from_col = from_col
      @from_row = from_row
      @to_col = to_col
      @to_row = to_row
      @from_col_abs = "$" if from_col_abs
      @from_row_abs = "$" if from_row_abs
      @to_col_abs = "$" if to_col_abs
      @to_row_abs = "$" if to_row_abs
    end
    def to_s
      "#{@from_col_abs}#{@from_col}#{@from_row_abs}#{@from_row}:#{@to_col_abs}#{@to_col}#{@to_row_abs}#{@to_row}"
    end
    def contains(col, row)
      print "col: #{col}, row: #{row}, from_col/row: #{@from_col}/#{@from_row}, to_col/row: #{@to_col}/#{@to_row}\n"
      (@from_row <= row) && (row <= @to_row) && (@from_col <= col) && (col <= @to_col)
    end
  end
  
  class Cell
    attr_reader :value
    attr_reader :formula
    attr_reader :array_formula
    attr_accessor :sheet_name
    attr_accessor :row
    attr_accessor :col
    def initialize(value=nil, formula=nil, array_formula=false)
      debugger if (value.nil? && formula.nil?)
      @value = value
      @formula = formula
      @array_formula = array_formula
    end

    def sanity_check
      return if @value
      return unless (@row && @col)
      if @formula.is_a? Formula
        @formula.args.each do |a|
          if (a.is_a? Range) && (a.contains(@col, @row))
            raise RangeError, "Cell at #{@sheet_name}!#{@col}#{@row} contains formula #{@formula} that overlaps itself"
          end
        end
      elsif @formula.is_a? CellRef
        if (@sheet_name == @formula.sheet_name) && (@row == @formula.row) && (@col == @formula.col)
          raise RangeError, "CellRef at #{@formula} refers to itself"
        end
      end
    end
  end

  class Sheet
    attr_reader :name
    attr_reader :header_rows
    attr_reader :rows
    attr_reader :columns
    attr_reader :frozen_row
    attr_reader :frozen_col
    def initialize(name, columns=nil, header_rows=nil, rows=nil)
      @name = name.gsub(/[\[\]:\*\/\\]/, '_')
      if name.length > 31
        @name = "#{@name[0..27]}..."
      end
      @columns = columns || []
      @header_rows = header_rows || []
      @rows = rows || []
    end

    def push_row(i, values)
      push_into(@rows, i, values)
    end

    def push_header_row(i, values)
      push_into(@header_rows, i, values)
    end

    def assign_coords
      @columns.each_with_index do |cell, c|
        cell.col = Spreadsheet.col_name(c)
      end
      @header_rows.each_with_index do |row, r|
        row.each_with_index do |cell, c|
          cell.sheet_name = @name
          cell.row = r + 1 + 1 # 1 for header row, and 1 for 1-based indexing
          cell.col = Spreadsheet.col_name(c)
        end
      end
      @rows.each_with_index do |row, r|
        row.each_with_index do |cell, c|
          cell.sheet_name = @name
          cell.row = r + @header_rows.length + 1 + 1 # 1 for header row, and 1 for 1-based indexing
          cell.col = Spreadsheet.col_name(c)
        end
      end
    end

    def freeze_panes(row, col)
      @frozen_row = row
      @frozen_col = col
    end

    def sanity_check
      @header_rows.each do |r|
        r.each do |c| c.sanity_check end
      end
      @rows.each do |r|
        r.each do |c| c.sanity_check end
      end
    end


    protected
    def to_cell(val)
      if val.instance_of?(Cell)
        val
      else
        Cell.new(val)
      end
    end

    def push_into(arr, i, vals)
      unless vals.instance_of?(Array)
        vals = [vals]
      end
      if i.nil?
        row = vals.map{|v| to_cell(v)}
        arr.push(row)
        row
      else
        vals.map{|v| arr[i].push(to_cell(v)) }
        arr[i]
      end
    end
  end

  def sanitize(str)
    return "" unless str.is_a? String
    str.gsub(/^\s*[-+=@]+/, "").strip
  end

  def to_xlsx
    io = StringIO.new
    @workbook = WriteXLSX.new(io)

    @twoPct = @workbook.add_format
    @twoPct.set_num_format("0.00%")
    @rawString = @workbook.add_format(num_format: "@")
    def render_row(ws, s, r, r_num, row_offset)
      r&.each_with_index do |c, c_num|
        if s.columns[c_num]&.type == "Percent"
          format = @twoPct
        elsif s.columns[c_num]&.type == "String"
          format = @rawString
        else
          format = nil
        end
        if c.formula
          if c.array_formula
            ws.write_formula(r_num + row_offset, c_num, "{=#{c.formula}}", format)
          else
            ws.write_formula(r_num + row_offset, c_num, "=#{c.formula}", format)
          end
        elsif c.value.is_a? Numeric
          ws.write_number(r_num + row_offset, c_num, c.value, format)
        elsif (c.value.is_a? DateTime) || (c.value.is_a? Time)
          ws.write_date_time(r_num + row_offset, c_num, c.value.to_formatted_s(:db), format)
        elsif c.value.is_a? Chart
          c.value.insert_into(@workbook, ws, CellRef.new(nil,
                                                         Spreadsheet.col_name(c_num), false,
                                                         r_num + row_offset, false, ""))
        else
          ws.write_string(r_num + row_offset, c_num, c.value, format)
        end
      end
    end
    @sheets.each do |s|
      ws = @workbook.add_worksheet(s.name)
      row_offset = 0
      s.columns.each_with_index do |c, c_num|
        ws.write_string(0 + row_offset, c_num, sanitize(c.name))
      end
      row_offset += 1
      s.header_rows.each_with_index do |r, r_num|
        render_row(ws, s, r, r_num, row_offset)
      end
      row_offset += s.header_rows.count
      s.rows.each_with_index do |r, r_num|
        render_row(ws, s, r, r_num, row_offset)
      end
      ws.freeze_panes(s.frozen_row, s.frozen_col)
    end
    @workbook.close
    io.string
  end
end
