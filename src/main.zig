const std = @import("std");
const testing = std.testing;

const perfectHash = @import("./perfect_hash.zig").perfectHash;

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

const base256Alphabet = [256][]const u8{ "\u{1F680}", "\u{1FA90}", "\u{2604}", "\u{1F6F0}", "\u{1F30C}", "\u{1F311}", "\u{1F312}", "\u{1F313}", "\u{1F314}", "\u{1F315}", "\u{1F316}", "\u{1F317}", "\u{1F318}", "\u{1F30D}", "\u{1F30F}", "\u{1F30E}", "\u{1F409}", "\u{2600}", "\u{1F4BB}", "\u{1F5A5}", "\u{1F4BE}", "\u{1F4BF}", "\u{1F602}", "\u{2764}", "\u{1F60D}", "\u{1F923}", "\u{1F60A}", "\u{1F64F}", "\u{1F495}", "\u{1F62D}", "\u{1F618}", "\u{1F44D}", "\u{1F605}", "\u{1F44F}", "\u{1F601}", "\u{1F525}", "\u{1F970}", "\u{1F494}", "\u{1F496}", "\u{1F499}", "\u{1F622}", "\u{1F914}", "\u{1F606}", "\u{1F644}", "\u{1F4AA}", "\u{1F609}", "\u{263A}", "\u{1F44C}", "\u{1F917}", "\u{1F49C}", "\u{1F614}", "\u{1F60E}", "\u{1F607}", "\u{1F339}", "\u{1F926}", "\u{1F389}", "\u{1F49E}", "\u{270C}", "\u{2728}", "\u{1F937}", "\u{1F631}", "\u{1F60C}", "\u{1F338}", "\u{1F64C}", "\u{1F60B}", "\u{1F497}", "\u{1F49A}", "\u{1F60F}", "\u{1F49B}", "\u{1F642}", "\u{1F493}", "\u{1F929}", "\u{1F604}", "\u{1F600}", "\u{1F5A4}", "\u{1F603}", "\u{1F4AF}", "\u{1F648}", "\u{1F447}", "\u{1F3B6}", "\u{1F612}", "\u{1F92D}", "\u{2763}", "\u{1F61C}", "\u{1F48B}", "\u{1F440}", "\u{1F62A}", "\u{1F611}", "\u{1F4A5}", "\u{1F64B}", "\u{1F61E}", "\u{1F629}", "\u{1F621}", "\u{1F92A}", "\u{1F44A}", "\u{1F973}", "\u{1F625}", "\u{1F924}", "\u{1F449}", "\u{1F483}", "\u{1F633}", "\u{270B}", "\u{1F61A}", "\u{1F61D}", "\u{1F634}", "\u{1F31F}", "\u{1F62C}", "\u{1F643}", "\u{1F340}", "\u{1F337}", "\u{1F63B}", "\u{1F613}", "\u{2B50}", "\u{2705}", "\u{1F97A}", "\u{1F308}", "\u{1F608}", "\u{1F918}", "\u{1F4A6}", "\u{2714}", "\u{1F623}", "\u{1F3C3}", "\u{1F490}", "\u{2639}", "\u{1F38A}", "\u{1F498}", "\u{1F620}", "\u{261D}", "\u{1F615}", "\u{1F33A}", "\u{1F382}", "\u{1F33B}", "\u{1F610}", "\u{1F595}", "\u{1F49D}", "\u{1F64A}", "\u{1F639}", "\u{1F5E3}", "\u{1F4AB}", "\u{1F480}", "\u{1F451}", "\u{1F3B5}", "\u{1F91E}", "\u{1F61B}", "\u{1F534}", "\u{1F624}", "\u{1F33C}", "\u{1F62B}", "\u{26BD}", "\u{1F919}", "\u{2615}", "\u{1F3C6}", "\u{1F92B}", "\u{1F448}", "\u{1F62E}", "\u{1F646}", "\u{1F37B}", "\u{1F343}", "\u{1F436}", "\u{1F481}", "\u{1F632}", "\u{1F33F}", "\u{1F9E1}", "\u{1F381}", "\u{26A1}", "\u{1F31E}", "\u{1F388}", "\u{274C}", "\u{270A}", "\u{1F44B}", "\u{1F630}", "\u{1F928}", "\u{1F636}", "\u{1F91D}", "\u{1F6B6}", "\u{1F4B0}", "\u{1F353}", "\u{1F4A2}", "\u{1F91F}", "\u{1F641}", "\u{1F6A8}", "\u{1F4A8}", "\u{1F92C}", "\u{2708}", "\u{1F380}", "\u{1F37A}", "\u{1F913}", "\u{1F619}", "\u{1F49F}", "\u{1F331}", "\u{1F616}", "\u{1F476}", "\u{1F974}", "\u{25B6}", "\u{27A1}", "\u{2753}", "\u{1F48E}", "\u{1F4B8}", "\u{2B07}", "\u{1F628}", "\u{1F31A}", "\u{1F98B}", "\u{1F637}", "\u{1F57A}", "\u{26A0}", "\u{1F645}", "\u{1F61F}", "\u{1F635}", "\u{1F44E}", "\u{1F932}", "\u{1F920}", "\u{1F927}", "\u{1F4CC}", "\u{1F535}", "\u{1F485}", "\u{1F9D0}", "\u{1F43E}", "\u{1F352}", "\u{1F617}", "\u{1F911}", "\u{1F30A}", "\u{1F92F}", "\u{1F437}", "\u{260E}", "\u{1F4A7}", "\u{1F62F}", "\u{1F486}", "\u{1F446}", "\u{1F3A4}", "\u{1F647}", "\u{1F351}", "\u{2744}", "\u{1F334}", "\u{1F4A3}", "\u{1F438}", "\u{1F48C}", "\u{1F4CD}", "\u{1F940}", "\u{1F922}", "\u{1F445}", "\u{1F4A1}", "\u{1F4A9}", "\u{1F450}", "\u{1F4F8}", "\u{1F47B}", "\u{1F910}", "\u{1F92E}", "\u{1F3BC}", "\u{1F975}", "\u{1F6A9}", "\u{1F34E}", "\u{1F34A}", "\u{1F47C}", "\u{1F48D}", "\u{1F4E3}", "\u{1F942}" };

test "basic add functionality" {
    // @setEvalBranchQuota(1000000000);
    // _ = perfectHash(&base256Alphabet);
    @setEvalBranchQuota(100000);
    _ = perfectHash(&[_][]const u8{
        "one",
        "two",
        "three",
        "four",
        "five",
    });
    try testing.expect(add(3, 7) == 10);
}