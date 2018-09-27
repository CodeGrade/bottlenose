require 'yaml'
require 'active_support/inflector'

class SchemaChecker
  def check(yaml)
    @path = []
    @checker["start"][:check].call(yaml, [], @checker).map!{|s| s.gsub(/^: /, "")}
  end
  def convert(yaml)
    @checker["start"][:convert].call(yaml, @checker)
  end
  def initialize(schema)
    schema =
      case schema
      when String
        YAML.load(schema)
      when Pathname
        YAML.load(File.read(schema))
      when File
        YAML.load(schema.read)
      when YAML
        schema
      else
        raise "Invalid argument type to initialize SchemaChecker: #{schema}"
      end

    @checker = {}
    @checker["String"] = {
      check: lambda {|val, parent, refinement=nil|
        if val.is_a? String
          if refinement.is_a? String
            refinement = Regexp.new(refinement)
            if refinement.match? val
              []
            else
              ["#{make_path}: Expected a string matching #{refinement.inspect}, but got #{elide(val, 2)}"]
            end
          else
            []
          end
        else
          ["#{make_path}: Expected a string, but got #{elide(val, 2)}"]
        end},
      convert: lambda {|val| val}
    }
    @checker["Boolean"] = {
      check: lambda {|val, parent, refinement=nil|
        if (val == true || val == false)
          []
        else
          ["#{make_path}: Expected a boolean, but got #{elide(val, 2)}"]
        end},
      convert: lambda {|val| val}
    }
    @checker["Integer"] = {
      check: lambda {|val, parent, refinement=nil|
        val = (Integer(val) rescue false)
        if val
          if (refinement.is_a? Array) && (refinement.size == 2)
            lo = get_number(parent, refinement[0])
            hi = get_number(parent, refinement[1])
            if hi && lo
              if lo <= val && val < hi
                []
              else
                ["#{make_path}: Expected an integer in the range [#{lo}, #{hi - 1}], but got #{elide(val, 2)}"]
              end
            else
              []
            end
          else
            []
          end
        else
          ["#{make_path}: Expected an integer, but got #{elide(val, 2)}"]
        end},
      convert: lambda {|val| Integer(val) }
    }
    @checker["Float"] = {
      check: lambda {|val, parent, refinement=nil|
        val = (Float(val) rescue false)
        if val
          if refinement.is_a? Array
            lo = get_number(parent, refinement[0])
            hi = get_number(parent, refinement[1])
            if lo <= val && val <= hi
              []
            else
              ["#{make_path}: Expected a float in the range [#{lo}, #{hi}], but got #{elide(val, 2)}"]
            end
          else
            []
          end
        else
          ["#{make_path}: Expected a float, but got #{elide(val, 2)}"]
        end},
      convert: lambda {|val| Float(val) }
    }
    start = schema.delete "start"
    schema.each do |name, type|
      @checker[name] = produce(type, name)
    end
    @checker["start"] = @checker[start]
  end

  protected


  def get_number(parent, refinement)
    if (refinement.is_a? Numeric)
      refinement
    elsif (parent[refinement].is_a? Numeric)
      parent[refinement]
    elsif parent[refinement].is_a? Array
      parent[refinement].size
    else
      nil
    end
  end

  def make_path
    op = nil
    new_path = []
    name_offset = -1
    @path.each do |p|
      case op
      when :named
        if new_path[name_offset].empty?
          new_path << p
        else
          new_path[name_offset] = "#{new_path[name_offset]} (\"#{p}\")"
        end
        op = nil
      when :anon
        name_offset -= 1
        new_path << p
        op = nil
      when :ignore
        op = nil
      # do nothing
      else
        if p.is_a? Symbol
          op = p
        else
          op = nil
          new_path << p
        end
      end
    end
    new_path.compact.join(" > ")
  end

  def options(arr, joiner="or")
    if arr.size > 1
      arr[0...-1].map{|i| "`#{i}`"}.join(', ') + " #{joiner} `#{arr[-1]}`"
    else
      arr[0]
    end
  end

  def in_path(p)
    @path += p
    yield
    @path.pop(p.size)
  end

  def do_check(type, val, parent)
    if type.is_a? String
      @checker[type][:check].call(val, parent)
    elsif type.is_a? Array
      @checker[type[0]][:check].call(val, parent, type[1])
    end
  end
  def produce(type, typename)
    if type["array"]
      {
        check: lambda {|val, parent, refinement=nil|
          if !val.is_a? Array
            ["#{make_path}: Expected an array of #{type["array"].pluralize}, but got #{elide(val, 2)}"]
          else
            errors = []
            val.each_with_index do |v, v_index|
              in_path ["#{type["array"]} #{v_index + 1}"] do
                errors.push(*do_check(type["array"], v, val))
              end
            end
            errors
          end},
        convert: lambda{|val|
          val.map do |v|
            @checker[type["array"]][:convert].call(v)
          end
        }
      }
    elsif type["dictionary"]
      {
        check: lambda {|val, parent, refinement=nil|
          errors = []
          if !val.is_a? Hash
            errors << "#{make_path}: Expected a dictionary, but got #{elide(val, 2)}"
          else
            val.each do |k, v|
              in_path ["#{k} key"] do
                errors.push(*do_check(type["dictionary"]["key"], k, val))
              end
              in_path ["#{k}"] do
                errors.push(*do_check(type["dictionary"]["value"], v, val))
              end
            end
          end
          errors
        },
        convert: lambda {|val|
          val.map do |k, v|
            [@checker[type["dictionary"]["key"]][:convert].call(k),
             @checker[type["dictionary"]["type"]][:convert].call(v)]
          end.to_h
        }
      }
    elsif type["labeled_value"]
      {
        check: lambda {|val, parent, refinement=nil|
          errors = []
          if (!val.is_a? Hash) || val.size != 1
            errors << "#{make_path}: Expected a #{typename}, but got #{elide(val, 2)}"
          else
            in_path ["label"] do
              errors.push(*do_check(type["labeled_value"]["label"], val.first[0], parent))
            end
            in_path [:named, "#{val.first[0]}"] do
              errors.push(*do_check(type["labeled_value"]["value"], val.first[1], parent))
            end
          end
          errors
        },
        convert: lambda {|val|
          val.map do |k, v|
            [k, @checker[type["labeled_value"]["value"]][:convert].call(v)]
          end.to_h
        }
      }
    elsif type["object"]
      {
        check: lambda {|val, parent, refinement=nil|
          errors = []
          checkers = type["object"].map do |field|
            [field["key"], {count: 0, optional: field["optional"], value: field["value"]}]
          end.to_h
          if !val.is_a? Hash
            errors << "#{make_path}: Expected an object, but got #{elide(val, 2)}"
          else
            named = 
              if val["name"]
                [:named, val["name"]]
              else
                []
              end
            in_path named do
              val.each do |k, v|
                if checkers[k]
                  in_path ["#{k}"] do
                    errors.push(*do_check(checkers[k][:value], v, val))
                    checkers[k][:count] += 1
                  end
                else
                  errors << "#{make_path}: Got an unexpected field #{k} in #{elide(val, 2)}.  Valid fields are #{options(checkers.keys)}"
                end
              end
              required = checkers.select{|k, v| !v[:optional] && v[:count] == 0}
              if !required.empty?
                errors << "#{make_path}: Missing required #{'field'.pluralize(required.size)} #{options(required.keys, 'and')}"
              end
            end
            errors
          end
        },
        convert: lambda {|val|
          converters = type["object"].map do |field|
            [field["key"], field["value"]]
          end.to_h
          val.map do |k, v|
            [k, @checker[converters[k]][:convert].call(v)]
          end.to_h
        }
      }
    elsif type["one_of"]
      {
        check: lambda {|val, parent, refinement=nil|
          type["one_of"].each do |typ|
            if do_check(typ, val, parent).empty?
              return []
            end
          end
          ["#{make_path}: Expected one of #{options(type["one_of"])}, got #{elide(val, 2)}"]
        },
        convert: lambda {|val|
          type["one_of"].each do |typ|
            if @checker[typ][:check].call(val, "").empty?
              return @checker[typ][:convert].call(val)
            end
          end
        }
      }
    elsif type["tagged_one_of"]
      {
        check: lambda {|val, parent, refinement=nil|
          key_str = options(type["tagged_one_of"])
          if !val.is_a? Hash
            ["#{make_path}: Expected an object tagged as #{key_str}"]
          elsif val.size > 1
            ["#{make_path}: Expected a single object, tagged as #{key_str}"]
          elsif !(type["tagged_one_of"].member? val.first[0])
            ["#{make_path}: Got unknown type #{val.first[0]}.  Expected an object tagged as #{key_str}"]
          else
            do_check(val.first[0], val.first[1], parent)
          end
        },
        convert: lambda {|val|
          @checker[val.first[0]][:convert].call(val.first[1])
        }
      }
    end
  end

  def elide(val, depth)
    if val.is_a? Hash
      if depth <= 0
        "{...}"
      else
        "{" + val.map do |k, v|
          "#{k.inspect}=>#{elide(v, depth - 1)}"
        end.join(", ") + "}"
      end
    elsif val.is_a? Array
      if depth <= 0
        "[...]"
      else
        "[#{val.map{|v| elide(v, depth - 1)}.join(", ")}]"
      end
    else
      val.inspect
    end
  end
end
