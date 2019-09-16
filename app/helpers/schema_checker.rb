require 'yaml'
require 'active_support/inflector'
require 'nokogiri'

class SchemaChecker
  def check(yaml)
    @path = []
    @commit = nil
    @checker["start"][:check].call(yaml, []).map!{|s| s.gsub(/^: /, "")}
  end
  def convert(yaml)
    @path = []
    @commit = nil
    @checker["start"][:convert].call(yaml)
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
    @checker["HTML"] = @checker["String"] # NOTE: This is temporary, and doesn't actually do any HTML validation
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
        val_clean = (Integer(val) rescue false)
        if val_clean
          if (refinement.is_a? Array) && (refinement.size == 2)
            lo = get_number(parent, refinement[0])
            hi = get_number(parent, refinement[1])
            if hi && lo
              if lo <= val_clean && val_clean < hi
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
        val_clean = (Float(val) rescue false)
        if val_clean
          if refinement.is_a? Array
            lo = get_number(parent, refinement[0])
            hi = get_number(parent, refinement[1])
            if lo && hi
              if lo <= val_clean && val_clean <= hi
                []
              else
                ["#{make_path}: Expected a float in the range [#{lo}, #{hi}], but got #{elide(val, 2)}"]
              end
            else
              []
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
        if new_path[name_offset].blank?
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
      "`#{arr[0]}`"
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
  def do_convert(type, val)
    if type.is_a? String
      @checker[type][:convert].call(val)
    elsif type.is_a? Array
      @checker[type[0]][:convert].call(val)
    end
  end
  def produce(type, typename)
    if type["array"]
      {
        check: lambda {|val, parent, refinement=nil|
          errors = []
          if !val.is_a? Array
            errors << "#{make_path}: Expected an array of #{type["array"].pluralize}, but got #{elide(val, 2)}"
          else
            if type["size"] && val.size != type["size"]
              errors << "#{make_path}: Expected an array with exactly #{type['size']} #{'item'.pluralize(type['size'])}"
            end
            if type["minSize"] && val.size < type["minSize"]
              errors << "#{make_path}: Expected an array with at least #{type['minSize']} #{'item'.pluralize(type['minSize'])}"
            end
            if type["maxSize"] && val.size > type["maxSize"]
              errors << "#{make_path}: Expected an array with at most #{type['maxSize']} #{'item'.pluralize(type['maxSize'])}"
            end
            val.each_with_index do |v, v_index|
              in_path ["#{type["array"]} #{v_index + 1}"] do
                errors.push(*do_check(type["array"], v, val))
              end
            end
          end
          errors
        },
        convert: lambda{|val|
          val.map do |v|
            do_convert(type["array"], v)
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
            if type["dictionary"]["size"] && val.size != type["dictionary"]["size"]
              errors << "#{make_path}: Expected an object with exactly #{type['dictionary']['size']} #{'key'.pluralize(type['dictionary']['size'])}"
            end
            if type["dictionary"]["minSize"] && val.size < type["dictionary"]["minSize"]
              errors << "#{make_path}: Expected an object with at least #{type['dictionary']['minSize']} #{'key'.pluralize(type['dictionary']['minSize'])}"
            end
            if type["dictionary"]["maxSize"] && val.size > type["dictionary"]["maxSize"]
              errors << "#{make_path}: Expected an object with at most #{type['dictionary']['maxSize']} #{'key'.pluralize(type['dictionary']['maxSize'])}"
            end
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
            [do_convert(type["dictionary"]["key"], k),
             do_convert(type["dictionary"]["value"], v)]
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
            [k, do_convert(type["labeled_value"]["value"], v)]
          end.to_h
        }
      }
    elsif type["object"]
      {
        check: lambda {|val, parent, refinement=nil|
          errors = []
          checkers = type["object"].map do |field|
            [field["key"], {count: 0, commit: field["commit"], optional: field["optional"], value: field["value"]}]
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
                k = k.to_s if k.is_a? Symbol
                if checkers[k]
                  @commit = make_path if checkers[k][:commit]
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
            [k, do_convert(converters[k], v)]
          end.to_h
        }
      }
    elsif type["one_of"]
      {
        check: lambda {|val, parent, refinement=nil|
          commit = @commit
          @commit = nil
          errors = []
          errors << "#{make_path}: Expected one of #{options(type["one_of"])}, got #{elide(val, 2)}"
          type["one_of"].each do |typ|
            as_typ = do_check(typ, val, parent)
            if as_typ.empty?
              @commit = commit
              return []
            elsif @commit
              @commit = commit
              return as_typ
            else
              msg_prefix = /#{make_path}\s*.*?(?:> |: )?/
              as_typ.each do |msg|
                errors << "\tIf this is a #{typ}, then: #{msg.gsub(msg_prefix, '')}"
              end
            end
          end
          @commit = commit
          errors
        },
        convert: lambda {|val|
          type["one_of"].each do |typ|
            if do_check(typ, val, "").empty?
              return do_convert(typ, val)
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
          [[val.first[0], do_convert(val.first[0], val.first[1])]].to_h
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
