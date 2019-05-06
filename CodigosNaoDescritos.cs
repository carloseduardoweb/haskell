using System;
using System.Collections.Generic;

using static Tools.List;

using Cod = System.ValueTuple<int, int, int>;
using Seq = System.Collections.Generic.List<(int, int, int)>;
using Unit = System.Collections.Generic.List<
              System.Collections.Generic.List<(int, int, int)>>;
using Tree = System.Collections.Generic.List<
              System.Collections.Generic.List<
                System.Collections.Generic.List<(int, int, int)>>>;

using CodFormat =  System.ValueTuple<int, int, int>;

public class CodigosNaoDescritos
{
  public static void Main(string[] args)
  {
    List<Cod> cods = new List<Cod> {(1,2,3),(1,2,5),(2,3,4),(3,1,4),(3,1,6),(3,3,5),(3,5,0),(3,5,3)};

    CodigosNaoDescritos cnd = new CodigosNaoDescritos();
    cnd.SortCods(cods); // se estiverem ordenados, não precisará
    List<Cod> gaps = cnd.GapsFromCods(cods);
    
    Console.WriteLine(String.Join(",", cods));
    Console.WriteLine(String.Join(",", cnd.FormatCods((2,6,3), cods)));
    
    Console.WriteLine(String.Join(",", gaps));
    Console.WriteLine(String.Join(",", cnd.FormatCods((2,6,3), gaps)));
  }

  private List<int> Gaps(List<int> nums)
  {
    void AddGaps(List<int> g, int init, int end)
    {
      if (init + 1 < end)
      {
        for (int i = init + 1; i < end; i++)
        {
          g.Add(i);
        }
      }
    }

    List<int> gaps = new List<int>();

    if (nums.Count == 0) 
    {
      return gaps;
    }

    AddGaps(gaps, 0, nums[0]);

    for (int i = 1; i < nums.Count; i++)
    {    
      AddGaps(gaps, nums[i - 1], nums[i]);
    }

    return gaps;
  }

  private int CompareCod(Cod cod1, Cod cod2)
  {
    int result;

    if ((result = cod1.Item1.CompareTo(cod2.Item1)) == 0)
    {
      if ((result = cod1.Item2.CompareTo(cod2.Item2)) == 0)
      {
        return cod1.Item3.CompareTo(cod2.Item3);
      }
    }

    return result;
  }

  public void SortCods(List<Cod> cods)
  {
    cods.Sort((Cod cod1, Cod cod2) => CompareCod(cod1, cod2));
  }

  private int CompareSeq(Seq seq1, Seq seq2)
  {
    if (IsNullOrEmptyList(seq1) && IsNullOrEmptyList(seq2)) return 0;
    if (IsNullOrEmptyList(seq1)) return -1;
    if (IsNullOrEmptyList(seq2)) return 1;
    
    return seq1[0].Item2.CompareTo(seq2[0].Item2);
  }

  private void SortSeqs(Unit unit)
  {
    unit.Sort((Seq seq1, Seq seq2) => CompareSeq(seq1, seq2));
  }

  public List<String> GapsFromCods(CodFormat fmt, List<String> strCods)
  {
    return FormatCods(fmt, GapsFromCods(MapList(strCods, (string strCod) => StringToCod(strCod?.Trim()))));
  }

  public List<Cod> GapsFromCods(List<Cod> cods)
  {
    if (IsNullOrEmptyList(cods)) return new List<Cod>();

    return TreeToCods(MapTreeToGaps(CodsToTree(cods)));
  }

  private Tree CodsToTree(List<Cod> cods)
  {
    if (IsNullOrEmptyList(cods)) return new Tree();

    return ListComprehension(GroupByUnit(cods), (Seq seq1) => ListComprehension(GroupBySequence(seq1), (Seq seq2) => seq2));
  }

  private Unit GroupByUnit(List<Cod> cods)
  {
    return GroupBy((Cod cod1, Cod cod2) => cod1.Item1 == cod2.Item1, cods);
  }

  private Unit GroupBySequence(List<Cod> cods)
  {
    return GroupBy((Cod cod1, Cod cod2) => cod1.Item2 == cod2.Item2, cods);
  }

  private Unit GroupBy(Func<Cod, Cod, bool> equiparator, List<Cod> cods)
  {
    Unit unit = new Unit();

    if (IsNullOrEmptyList(cods))
    {
      return unit;
    }

    Seq seq = new Seq();
    unit.Add(seq);
    seq.Add(cods[0]);

    for (int i = 1; i < cods.Count; i++)
    {
      if (!equiparator(cods[i - 1], cods[i]))
      {
        seq = new Seq();
        unit.Add(seq);
      }
      
      seq.Add(cods[i]);
    }

    return unit;
  }

  private Tree MapTreeToGaps(Tree tree)
  {
    return MapTreeToSeqGaps(MapTreeToExtGaps(tree));
  }

  private Tree MapTreeToExtGaps(Tree tree)
  {
    return MapList(tree, (Unit unit) => MapUnitToExtGaps(unit));
  }

  private Unit MapUnitToExtGaps(Unit unit)
  {
    return MapList(unit, (Seq seq) => ListComprehension(Gaps(ListExt(seq)), (int e) => (seq[0].Item1, seq[0].Item2, e)));
  }

  private List<int> ListExt(Seq seq)
  {
    return ListComprehension(seq, (Cod cod) => cod.Item3);
  }

  private Tree MapTreeToSeqGaps(Tree tree)
  {
    return MapList(tree, (Unit unit) => MergeUnits(unit, ListComprehension(Gaps(ListSeq(unit)), (int s) => new Seq {(unit[0][0].Item1, s, 0)})));
  }

  private List<int> ListSeq(Unit unit)
  {
    return MapList(ListComprehension(unit, (Seq seq) => seq, (Seq seq) => !IsNullOrEmptyList(seq)), (Seq seq) => seq[0].Item2);
  }

  private Unit MergeUnits(Unit unit1, Unit unit2)
  {
    if (unit1 == null && unit2 == null) return new Unit();
    if (unit1 == null) return unit2;
    if (unit2 == null) return unit1;

    unit1.AddRange(unit2);
    SortSeqs(unit1);
    return unit1;
  }

  private List<Cod> TreeToCods(Tree tree)
  {
    List<Cod> cods = new List<Cod>();

    if (IsNullOrEmptyList(tree)) return cods;

    foreach (Unit u in tree)
    {
      if (IsNullOrEmptyList(u)) continue;

      foreach (Seq s in u)
      {
        if (IsNullOrEmptyList(s)) continue;

        foreach (Cod c in s)
        {
          cods.Add(c);
        }
      }
    }

    return cods;
  }

  public string FormatCod(CodFormat fmt, Cod cod)
  {
    return String.Format("{0}/{1}{2}", cod.Item1.ToString($"D{fmt.Item1}")
                                    , cod.Item2.ToString($"D{fmt.Item2}")
                                    , (cod.Item3 == 0 ? "" : "-" + cod.Item3.ToString($"D{fmt.Item3}")));
  }

  public List<string> FormatCods(CodFormat fmt, List<Cod> cods)
  {
    return MapList(cods, (Cod cod) => FormatCod(fmt, cod));
  }

  public Cod StringToCod(string strCod)
  {
    string[] split = strCod?.Split(new Char [] {'/', '-'});
    int[] values = new int[] {0,0,0};

    for (int i = 0; i < (split?.Length ?? 0); i++)
    {
      values[i] = int.Parse(split[i]);
    }

    return (values[0], values[1], values[2]);
  }

  public List<Cod> StringsToCods(List<String> strCods)
  {
    return MapList(strCods, (string strCod) => StringToCod(strCod));
  }
}

namespace Tools
{ 
  class List
  {
    public static bool IsNullOrEmptyList<T>(List<T> list)
    {
      return (list?.Count ?? 0) == 0;    
    }

    public static List<U> MapList<T, U>(List<T> list, Func<T, U> func)
    {
      List<U> newList = new List<U>();

      foreach (T elem in list)
      {
        newList.Add(func(elem));
      }

      return newList;
    }

    public static List<U> ListComprehension<T, U>(List<T> list, Func<T, U> func, Func<T, bool> predicate = null)
    {
      List<U> newList = new List<U>();

      foreach (T elem in list)
      {
        if (predicate == null || predicate(elem))
        {
          newList.Add(func(elem));
        }
      }

      return newList;
    }
  }
}