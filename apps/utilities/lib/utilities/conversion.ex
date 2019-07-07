defmodule Utilities.Conversion do
  @moduledoc false
  require Utilities
  require Record

  def replace_all_bins_to_list(obj) do
    Utilities.for_each_non_iterable_item(obj, fn x ->
      if is_binary(x) do
        :binary.bin_to_list(x)
      else
        x
      end
    end)
  end

  def tuple_list_to_map(tp = [{_, _} | _]) when is_list(tp) do
    tp
    |> Enum.map(fn {k, v} ->
      if is_list(v) do
        {k, tuple_list_to_map(v)}
      else
        {k, v}
      end
    end)
    |> Map.new()
  end

  def tuple_list_to_map(tp) when is_list(tp) do
    tp
    |> Enum.map(fn x ->
      case x do
        [{_, _} | _] -> tuple_list_to_map(x)
        r when is_list(r) -> tuple_list_to_map(r)
        _ -> x
      end
    end)
  end

  def nested_map_to_tuple_list(l) when is_list(l) do
    l
    |> Enum.map(fn x ->
      is_list_map = {is_list(x), is_map(x)}

      case is_list_map do
        {true, _} -> nested_map_to_tuple_list(x)
        {_, true} -> nested_map_to_tuple_list(x)
        {false, false} -> x
      end
    end)
  end

  def nested_map_to_tuple_list(map) when is_map(map) do
    Map.to_list(map)
    |> Enum.map(fn {k, v} ->
      is_list_map = {is_list(v), is_map(v)}

      r =
        case is_list_map do
          {true, _} -> nested_map_to_tuple_list(v)
          {_, true} -> nested_map_to_tuple_list(v)
          {false, false} -> v
        end

      {k, r}
    end)
  end

  def record_to_map(record) when Record.is_record(record) do
    record_name = record |> elem(0)

    mod_name =
      case record_name do
        :diameter_packet ->
          OnlineChargingSystem.Records.Diameter
      end

    Kernel.apply(mod_name, :record_to_map, [record])
  end

  # def map_to_record(record_kind, map) do
  #   Utilities.Conversion.Protocols.RecordMapConversion.map_to_record(record_kind, map)
  # end

  @compile {:inline, ip_address_tuple_to_string: 1}
  def ip_address_tuple_to_string(ip_address_tuple) when is_tuple(ip_address_tuple) do
    Utilities.erl_list_to_iex_string(:inet_parse.ntoa(ip_address_tuple))
  end

  @spec integer_to_inet_binary(integer(), integer()) :: binary()
  @compile {:inline, integer_to_inet_binary: 2}
  def integer_to_inet_binary(number, bin_length)
      when is_integer(number) and is_integer(bin_length) do
    bit_size = 8 * bin_length
    <<number::size(bit_size)>>
  end

  def nested_tuple_to_list(list) when is_list(list) do
    list
    |> Enum.map(&nested_tuple_to_list/1)
  end

  def nested_tuple_to_list(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.map(&nested_tuple_to_list/1)
  end

  def nested_tuple_to_list(map) when is_map(map) do
    Enum.reduce(
      Map.to_list(map),
      %{},
      fn {k, v}, acc ->
        new_v =
          if is_list(v) or is_map(v) or is_tuple(v) do
            nested_tuple_to_list(v)
          else
            v
          end

        Map.put(acc, k, new_v)
      end
    )
  end

  def nested_tuple_to_list(x), do: x

  def convert_hex_to_unicode(text) do
    case text do
      n when n in [nil, "", " "] ->
        " "

      t ->
        t |> URI.decode_www_form() |> String.downcase()
    end
  end
end

defmodule Utilities.Conversion.Persian do
  @moduledoc """
  Convert word to persian.  use Persian.fix/1
"""
  @corrections [
      %{ list: ["؆","؇","؈","؉","؊","؍","؎","ؐ","ؑ","ؒ","ؓ","ؔ","ؕ",
          "ؖ","ؘ","ؙ","ؚ","؞","ٖ","ٗ","٘","ٙ","ٚ","ٛ","ٜ","ٝ","ٞ","ٟ","٪",
          "٬","٭","ہ","ۂ","ۃ","۔","ۖ","ۗ","ۘ","ۙ","ۚ","ۛ","ۜ","۞","۟","۠",
          "ۡ","ۢ","ۣ","ۤ","ۥ","ۦ","ۧ","ۨ","۩","۪","۫","۬","ۭ","ۮ","ۯ","ﮧ",
          "﮲","﮳","﮴","﮵","﮶","﮷","﮸","﮹","﮺","﮻","﮼","﮽","﮾","﮿","﯀","﯁","ﱞ",
          "ﱟ","ﱠ","ﱡ","ﱢ","ﱣ","ﹰ","ﹱ","ﹲ","ﹳ","ﹴ","ﹶ","ﹷ","ﹸ","ﹹ","ﹺ","ﹻ","ﹼ","ﹽ",
          "ﹾ","ﹿ"], rep: ""},

  %{list:  ["أ","إ","ٱ","ٲ","ٳ","ٵ","ݳ","ݴ","ﭐ","ﭑ","ﺃ","ﺄ","ﺇ","ﺈ",
          "ﺍ","ﺎ","𞺀","ﴼ","ﴽ","𞸀"], rep: "ا"},

  %{list: ["ٮ","ݕ","ݖ","ﭒ","ﭓ","ﭔ","ﭕ","ﺏ","ﺐ","ﺑ","ﺒ","𞸁","𞸜",
          "𞸡","𞹡","𞹼","𞺁","𞺡"], rep: "ب"},

  %{list: ["ڀ","ݐ","ݔ","ﭖ","ﭗ","ﭘ","ﭙ","ﭚ","ﭛ","ﭜ","ﭝ"], rep: "پ"},
  %{list: ["ٹ","ٺ","ٻ","ټ","ݓ","ﭞ","ﭟ","ﭠ","ﭡ","ﭢ","ﭣ","ﭤ","ﭥ",
          "ﭦ","ﭧ","ﭨ","ﭩ","ﺕ","ﺖ","ﺗ","ﺘ","𞸕","𞸵","𞹵","𞺕","𞺵"], rep: "ت"},
  %{list: ["ٽ","ٿ","ݑ","ﺙ","ﺚ","ﺛ","ﺜ","𞸖","𞸶","𞹶","𞺖","𞺶"],  rep: "ث"},
  %{list: ["ڃ","ڄ","ﭲ","ﭳ","ﭴ","ﭵ","ﭶ","ﭷ","ﭸ","ﭹ","ﺝ","ﺞ","ﺟ",
          "ﺠ","𞸂","𞸢","𞹂","𞹢","𞺂","𞺢"], rep: "ج"},
  %{list: ["ڇ","ڿ","ݘ","ﭺ","ﭻ","ﭼ","ﭽ","ﭾ","ﭿ","ﮀ","ﮁ",
          "𞸃","𞺃"], rep: "چ"},
  %{list: ["ځ","ݮ","ݯ","ݲ","ݼ","ﺡ","ﺢ","ﺣ","ﺤ","𞸇","𞸧","𞹇","𞹧",
          "𞺇","𞺧"], rep: "ح"},
  %{list: ["ڂ","څ","ݗ","ﺥ","ﺦ","ﺧ","ﺨ","𞸗","𞸷","𞹗","𞹷","𞺗","𞺷"], rep: "خ"},
  %{list: ["ڈ","ډ","ڊ","ڌ","ڍ","ڎ","ڏ","ڐ","ݙ","ݚ","ﺩ","ﺪ","𞺣","ﮂ",
          "ﮃ","ﮈ","ﮉ"], rep: "د"},
  %{list: ["ﱛ","ﱝ","ﺫ","ﺬ","𞸘","𞺘","𞺸","ﮄ","ﮅ","ﮆ","ﮇ"], rep: "ذ"},
  %{list: ["٫","ڑ","ڒ","ړ","ڔ","ڕ","ږ","ݛ","ݬ","ﮌ","ﮍ","ﱜ","ﺭ","ﺮ",
          "𞸓","𞺓","𞺳"], rep: "ر"},
  %{list: ["ڗ","ڙ","ݫ","ݱ","ﺯ","ﺰ","𞸆","𞺆","𞺦"], rep: "ز"},
  %{list: ["ﮊ","ﮋ","ژ"], rep: "ژ"},
  %{list: ["ښ","ݽ","ݾ","ﺱ","ﺲ","ﺳ","ﺴ","𞸎","𞸮","𞹎","𞹮","𞺎","𞺮"], rep: "س"},
  %{list: ["ڛ","ۺ","ݜ","ݭ","ݰ","ﺵ","ﺶ","ﺷ","ﺸ","𞸔","𞸴","𞹔","𞹴",
          "𞺔","𞺴"], rep: "ش"},
  %{list: ["ڝ","ﺹ","ﺺ","ﺻ","ﺼ","𞸑","𞹑","𞸱","𞹱","𞺑","𞺱"], rep: "ص"},
  %{list: ["ڞ","ۻ","ﺽ","ﺾ","ﺿ","ﻀ","𞸙","𞸹","𞹙","𞹹","𞺙","𞺹"], rep: "ض"},
  %{list: ["ﻁ","ﻂ","ﻃ","ﻄ","𞸈","𞹨","𞺈","𞺨"], rep: "ط"},
  %{list: ["ڟ","ﻅ","ﻆ","ﻇ","ﻈ","𞸚","𞹺","𞺚","𞺺"], rep: "ظ"},
  %{list: ["؏","ڠ","ﻉ","ﻊ","ﻋ","ﻌ","𞸏","𞸯","𞹏","𞹯","𞺏","𞺯"], rep: "ع"},
  %{list: ["ۼ","ݝ","ݞ","ݟ","ﻍ","ﻎ","ﻏ","ﻐ","𞸛","𞸻","𞹛","𞹻","𞺛",
          "𞺻"], rep: "غ"},
  %{list: ["؋","ڡ","ڢ","ڣ","ڤ","ڥ","ڦ","ݠ","ݡ","ﭪ","ﭫ","ﭬ","ﭭ",
          "ﭮ","ﭯ","ﭰ","ﭱ","ﻑ","ﻒ","ﻓ","ﻔ","𞸐","𞸞","𞸰","𞹰","𞹾","𞺐","𞺰"], rep: "ف"},
  %{list: ["ٯ","ڧ","ڨ","ﻕ","ﻖ","ﻗ","ﻘ","𞸒","𞸟","𞸲","𞹒","𞹟","𞹲",
          "𞺒","𞺲","؈"], rep: "ق"},
  %{list: ["ػ","ؼ","ك","ڪ","ګ","ڬ","ڭ","ڮ","ݢ","ݣ","ݤ","ݿ","ﮎ",
          "ﮏ","ﮐ","ﮑ","ﯓ","ﯔ","ﯕ","ﯖ","ﻙ","ﻚ","ﻛ","ﻜ","𞸊","𞸪","𞹪"], rep: "ک"},
  %{list: ["ڰ","ڱ","ڲ","ڳ","ڴ","ﮒ","ﮓ","ﮔ","ﮕ","ﮖ","ﮗ","ﮘ","ﮙ","ﮚ",
          "ﮛ","ﮜ","ﮝ"], rep: "گ"},
  %{list: ["ڵ","ڶ","ڷ","ڸ","ݪ","ﻝ","ﻞ","ﻟ","ﻠ","𞸋","𞸫","𞹋","𞺋",
          "𞺫"], rep: "ل"},
  %{list: ["۾","ݥ","ݦ","ﻡ","ﻢ","ﻣ","ﻤ","𞸌","𞸬","𞹬","𞺌","𞺬"], rep: "م"},
  %{list: ["ڹ","ں","ڻ","ڼ","ڽ","ݧ","ݨ","ݩ","ﮞ","ﮟ","ﮠ","ﮡ","ﻥ","ﻦ",
          "ﻧ","ﻨ","𞸍","𞸝","𞸭","𞹍","𞹝","𞹭","𞺍","𞺭"], rep: "ن"},
  %{list: ["ؤ","ٶ","ٷ","ۄ","ۅ","ۆ","ۇ","ۈ","ۉ","ۊ","ۋ","ۏ","ݸ","ݹ",
          "ﯗ","ﯘ","ﯙ","ﯚ","ﯛ","ﯜ","ﯝ","ﯞ","ﯟ","ﯠ","ﯡ","ﯢ","ﯣ","ﺅ","ﺆ","ﻭ","ﻮ",
          "𞸅","𞺅","𞺥"], rep: "و"},
  %{list: ["ة","ھ","ۀ","ە","ۿ","ﮤ","ﮥ","ﮦ","ﮩ","ﮨ","ﮪ","ﮫ","ﮬ","ﮭ",
          "ﺓ","ﺔ","ﻩ","ﻪ","ﻫ","ﻬ","𞸤","𞹤","𞺄"], rep: "ه"},
  %{list: ["ؠ","ئ","ؽ","ؾ","ؿ","ى","ي","ٸ","ۍ","ێ","ې","ۑ","ے","ۓ",
          "ݵ","ݶ","ݷ","ݺ","ݻ","ﮢ","ﮣ","ﮮ","ﮯ","ﮰ","ﮱ","ﯤ","ﯥ","ﯦ","ﯧ","ﯨ",
          "ﯩ","ﯼ","ﯽ","ﯾ","ﯿ","ﺉ","ﺊ","ﺋ","ﺌ","ﻯ","ﻰ","ﻱ","ﻲ","ﻳ","ﻴ","𞸉","𞸩",
          "𞹉","𞹩","𞺉","𞺩"], rep: "ی"},
  %{list: ["ٴ","۽","ﺀ"], rep: "ء"},
  %{list: ["ﻵ","ﻶ","ﻷ","ﻸ","ﻹ","ﻺ","ﻻ","ﻼ"], rep: "لا"}]



defp fix_char(char) do
    case (@corrections |> Enum.filter(fn(data) ->
      data.list |> Enum.find_index(fn(c) ->
          c == char
      end) != nil
  end) |> List.last) do
          nil ->
              char
          [] ->
              char
          l ->
              l.rep
      end
end

defp get_word_chars(word) do
    for i <- 1..(word |> String.length) do
        word
          |> String.slice(i-1, 1)

     end |> Enum.map(fn(x) ->  x |> fix_char end)
end


def fix(word) do
      word |> get_word_chars |> Enum.join
end



end
