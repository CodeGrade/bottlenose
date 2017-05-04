
class Array
  def split_on(pat)
    as = [[]]
    ii = 0

    self.each do |xx|
      if xx.match(pat)
        as << []
        ii += 1
      else
        as[ii] << xx
      end
    end

    as
  end
end
