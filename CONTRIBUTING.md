# Contributing

Pull requests are welcome. This page collects the conventions that are not visible from the
source alone — everything else follows from the surrounding code.

## Build environment

The library targets **Delphi 10.4 Sydney or newer** for consumers, as stated in the readme.
Changes are built and verified with **Delphi 13 (Studio 37.0)**, Win64, Debug, so a change has
to compile there.

The two versions differ in a way that matters:

> `TList<T>.Count` is `NativeInt` from Delphi 13 on. On Delphi 11 it is still `Integer`.

That difference bites in exactly one place, and it has broken the build more than once:

```pascal
Assert.AreEqual(1, Descriptor.TradeLineItems.Count);        // E2532 on Delphi 13
Assert.AreEqual(1, Integer(Descriptor.TradeLineItems.Count)); // correct
```

Without the cast, `Assert.AreEqual<T>` has no common type argument and the unit fails with
`E2532`. Worse, the compiler then reports unrelated follow-up errors — typically `E2250` on
`WillRaise` calls thousands of lines further down — which point away from the real cause. On
Delphi 11 the same source compiles cleanly, so this is easy to miss.

**Rule: always write `Integer(<expression>.Count)` in `Assert.AreEqual`**, even where `Count`
is already `Integer`, as with `TStringList`. The source text does not reveal which one it is,
and the cast is free. `Unittest/Test-CountAssertions.ps1` enforces this.

## Language of comments

Code comments in this repository are written **in German, with correct umlauts**. Answering in
English is fine everywhere else — commit messages, pull request descriptions, issues.

The English section markers inherited from the C# port (`// Buyer (BG-7)`, `// Notes`, …) are
left as they are. They predate this convention; please do not translate them in either
direction, and please do not translate existing German comments.

## Encoding and line endings

- Source files are UTF-8. **Non-ASCII source requires a UTF-8 BOM** so that Delphi does not fall
  back to the system ANSI code page on a machine whose locale is not UTF-8. The library units
  carry a BOM; test units only need one once they contain non-ASCII characters.
- Line endings are **CRLF**.
- `Unittest/Test-SourceEncoding.ps1` checks the encoding rule across all tracked sources.

## Before opening a pull request

From `Unittest/`, after building `ZfDUnitTest.dproj` (Win64/Debug):

```powershell
.\Test-SourceEncoding.ps1     # UTF-8 and BOM
.\Test-CountAssertions.ps1    # the NativeInt Count rule
.\Test-Runner.ps1             # runner self-test plus the full Delphi suite
```

`Test-Runner.ps1` covers the other two, so it alone is enough before pushing. Exit codes, test
selection and the limits of the memory checks are documented in
[Unittest/README.md](Unittest/README.md).

Note that `Tests Leaked : 0` proves nothing — the default DUnitX monitor always reports zero.
Ownership work is verified with the explicit heap snapshots in `TZUGFeRDTestBase`.

## Diff hygiene

Please keep reindentation separate from behaviour changes. Wrapping an existing block in
`try … except` shifts every line inside it, and a change that is a few dozen lines of substance
can arrive as two thousand lines of diff. Reviewing it then means reading it with `git diff -w`
to find the actual change, and it conflicts with anything else in flight. Either commit the
reindentation on its own, or mention in the pull request which commit is pure whitespace.

## Relationship to the C# library

This is a port of [ZUGFeRD-csharp](https://github.com/stephanstapel/ZUGFeRD-csharp), and the
readme records the synchronization point. The C# library is a **reference, not a boundary**:
where a finding turns out to be a genuine defect, it is fixed here. "The same TODO exists in
C#" is not a reason to close a finding — it only shows that the port was faithful.

## License

Contributions are made under the Apache License 2.0, matching the headers in the source files.
