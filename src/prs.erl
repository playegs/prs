%% Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(prs).
-compile([native]).

-export([compress/1]).
-export([decompress/1]).

%% Compress.

compress(Data) ->
	compress(Data, <<>>, <<>>, [], <<>>).

%% End the compression.
compress(<<>>, _, Comp, Flags, Buffer) ->
	Flags2 = [1, 0|Flags],
	NbFlags = length(Flags2),
	Filler = if
		NbFlags > 8 -> 16 - NbFlags;
		true -> 8 - NbFlags
	end,
	{Comp2, Flags3, Buffer2} = copy_flags(Comp, Flags2, Buffer),
	CopyFlags = << << F:1 >> || F <- Flags3 >>,
	<< Comp2/binary, 0:Filler, CopyFlags/bits, Buffer2/binary, 0:16 >>;
%% Slide the window.
%%
%% We keep a window of 16#2000 but always ignore the first byte
%% in longest_common. This simplifies the code.
compress(Data, Window, Comp, Flags, Buffer)
		when byte_size(Window) > 16#2000 ->
	Slide = byte_size(Window) - 16#2000,
	slide_window(Data, Window, Comp, Flags, Buffer, Slide);
%% Not enough data anymore.
compress(Data, Window, Comp, Flags, Buffer)
		when byte_size(Data) < 3 ->
	copy_byte(Data, Window, Comp, Flags, Buffer);
%% Try compressing.
compress(Data, Window, Comp, Flags, Buffer)
		when byte_size(Window) > 3 ->
	{Size, Offset} = longest_common(Data, Window),
	Offset2 = Offset - byte_size(Window),
	copy(Data, Window, Comp, Flags, Buffer, Size, Offset2);
%% Not enough data in the window yet.
compress(Data, Window, Comp, Flags, Buffer) ->
	copy_byte(Data, Window, Comp, Flags, Buffer).

slide_window(Data, Window, Comp, Flags, Buffer, 0) ->
	compress(Data, Window, Comp, Flags, Buffer);
slide_window(Data, << _, Window/binary >>, Comp, Flags, Buffer, Slide) ->
	slide_window(Data, Window, Comp, Flags, Buffer, Slide - 1).

copy(Data, Window, Comp, Flags, Buffer, 0, _) ->
	copy_byte(Data, Window, Comp, Flags, Buffer);
copy(Data, Window, Comp, Flags, Buffer, Size, Offset)
		when Size =< 5, Offset > -256 ->
	copy_short(Data, Window, Comp, Flags, Buffer, Size, Offset);
copy(Data, Window, Comp, Flags, Buffer, Size, Offset) ->
	copy_long(Data, Window, Comp, Flags, Buffer, Size, Offset).

copy_byte(<< Byte, Rest/binary >>, Window, Comp, Flags, Buffer) ->
	case length(Flags) of
		8 ->
			Flags2 = << << F:1 >> || F <- Flags >>,
			compress(Rest, << Window/binary, Byte >>,
				<< Comp/binary, Flags2/binary, Buffer/binary >>,
				[1], << Byte >>);
		_ ->
			compress(Rest, << Window/binary, Byte >>,
				Comp, [1|Flags], << Buffer/binary, Byte >>)
	end.

copy_short(Data, Window, Comp, Flags, Buffer, Size, Offset) ->
	<< This:Size/binary, Rest/binary >> = Data,
	Size2 = Size - 2,
	A = Size2 bsr 1,
	B = Size2 band 1,
	{Comp2, Flags2, Buffer2} = copy_flags(Comp, [B, A, 0, 0|Flags], Buffer),
	compress(Rest, << Window/binary, This/binary >>,
		Comp2, Flags2, << Buffer2/binary, Offset >>).

copy_long(Data, Window, Comp, Flags, Buffer, Size, Offset) ->
	<< This:Size/binary, Rest/binary >> = Data,
	OffsetR = Offset band 16#1f,
	OffsetL = Offset bsr 5,
	{Comp2, Flags2, Buffer2} = copy_flags(Comp, [1, 0|Flags], Buffer),
	if
		Size =< 9 ->
			Size2 = Size - 2,
			compress(Rest, << Window/binary, This/binary >>, Comp2, Flags2,
				<< Buffer2/binary, OffsetR:5, Size2:3, OffsetL:8 >>);
		true ->
			Size2 = Size - 1,
			compress(Rest, << Window/binary, This/binary >>, Comp2, Flags2,
				<< Buffer2/binary, OffsetR:5, 0:3, OffsetL:8, Size2:8 >>)
	end.

copy_flags(Comp, Flags, Buffer) when length(Flags) =< 8 ->
	{Comp, Flags, Buffer};
copy_flags(Comp, Flags, Buffer) ->
	{Flags2, CopyFlags} = lists:split(length(Flags) - 8, Flags),
	CopyFlags2 = << << F:1 >> || F <- CopyFlags >>,
	{<< Comp/binary, CopyFlags2/binary, Buffer/binary >>,
		Flags2, <<>>}.

%% This function will unfortunately find the longest common segment the
%% furthest from the end of the sliding window, which might result in a
%% copy_long where we could have had a copy_short.
longest_common(Data, Window) ->
	longest_common(Data, Window, 3, 0, 0, 0).

%% End of window.
longest_common(_, <<>>, _, _, Size, Offset) ->
	{Size, Offset};
%% We don't have enough data in the buffer anymore.
longest_common(Data, _, CurSize, _, Size, Offset)
		when byte_size(Data) < CurSize ->
	{Size, Offset};
longest_common(Data, << _, Window/binary >>,
		CurSize, CurOffset, Size, Offset) ->
	longest_common_prefix(Data, Window, CurSize, CurOffset + 1, Size, Offset,
		0, Data, Window).

%% We got a segment of maximum size, 255 bytes.
longest_common_prefix(_, _, _, CurOffset, _, _, 255, _, _) ->
	{255, CurOffset};
%% We reached the end of the window. Do RLE emulation.
%%
%% RLE emulation uses data already decoded for copying, allowing us
%% to copy to an offset past the size of the current window.
longest_common_prefix(Data, Window, CurSize, CurOffset, Size, Offset,
		N, Data2, <<>>) when N >= CurSize ->
	longest_common_prefix(Data, Window, CurSize, CurOffset, Size, Offset,
		N, Data2, Data);
%% Match, continue.
longest_common_prefix(Data, Window, CurSize, CurOffset, Size, Offset,
		N, << B, Data2/binary >>, << B, Window2/binary >>) ->
	longest_common_prefix(Data, Window, CurSize, CurOffset, Size, Offset,
		N + 1, Data2, Window2);
%% No match, but we got a better prefix, save it and try next.
longest_common_prefix(Data, Window, CurSize, CurOffset, _, _, N, _, _)
		when N >= CurSize ->
	longest_common(Data, Window, N, CurOffset, N, CurOffset);
%% No match, no better prefix. Try next.
longest_common_prefix(Data, Window, CurSize, CurOffset, Size, Offset,
		_, _, _) ->
	longest_common(Data, Window, CurSize, CurOffset, Size, Offset).

%% Decompress.

decompress(Data) ->
	decompress([], Data, <<>>).

decompress([], << Flags:1/binary, Rest/binary >>, Window) ->
	Flags2 = lists:reverse([F || << F:1 >> <= Flags]),
	decompress(Flags2, Rest, Window);
decompress([1|Flags], << Byte:8, Rest/binary >>, Window) ->
	decompress(Flags, Rest, << Window/binary, Byte:8 >>);
decompress([0|Flags], Data, Window) ->
	search(Flags, Data, Window).

search([], << Flags:1/binary, Rest/binary >>, Window) ->
	Flags2 = lists:reverse([F || << F:1 >> <= Flags]),
	search(Flags2, Rest, Window);
search([1|_], << 0:16, _/bits >>, Window) ->
	Window;
%% Long search.
search([1|Flags], << OffsetR:5, Size:3, OffsetL:8, Rest/binary >>, Window) ->
	Offset = byte_size(Window) + (OffsetL bsl 5) + OffsetR - 16#2000,
	case Size of
		0 ->
			<< Size2:8, Rest2/binary >> = Rest,
			search_offset(Flags, Rest2, Window, Size2 + 1, Offset);
		_ ->
			search_offset(Flags, Rest, Window, Size + 2, Offset)
	end;
search([0|Flags], Data, Window) ->
	search_short(Flags, Data, Window).

search_short([], << Flags:1/binary, Rest/binary >>, Window) ->
	[A|Flags2] = lists:reverse([F || << F:1 >> <= Flags]),
	search_short(Flags2, Rest, Window, A);
search_short([A|Flags], Data, Window) ->
	search_short(Flags, Data, Window, A).

search_short([], << Flags:1/binary, Rest/binary >>, Window, A) ->
	[B|Flags2] = lists:reverse([F || << F:1 >> <= Flags]),
	search_short(Flags2, Rest, Window, A, B);
search_short([B|Flags], Data, Window, A) ->
	search_short(Flags, Data, Window, A, B).

search_short(Flags, << Byte:8, Rest/binary >>, Window, A, B) ->
	Size = B + A * 2 + 2,
	Offset = byte_size(Window) + Byte - 256,
	search_offset(Flags, Rest, Window, Size, Offset).

search_offset(Flags, Data, Window, 0, _) ->
	decompress(Flags, Data, Window);
search_offset(Flags, Data, Window, Size, Offset) ->
	case Window of
		<< _:Offset/binary, Copy:Size/binary, _/binary >> ->
			decompress(Flags, Data, << Window/binary, Copy/binary >>);
		<< _:Offset/binary, Copy/binary >> ->
			%% Reached the end of the window. Start RLE emulation.
			%%
			%% Part of the data being copied doesn't exist in the
			%% sliding window yet. Get the end and see how many times
			%% we must repeat the end to get our result.
			search_rle_emulation(Flags, Data, Window,
				Size, Copy, byte_size(Copy))
	end.

search_rle_emulation(Flags, Data, Window, Size, Copy, CopySize)
		when CopySize < Size ->
	search_rle_emulation(Flags, Data, << Window/binary, Copy/binary >>,
		Size - CopySize, Copy, CopySize);
search_rle_emulation(Flags, Data, Window, Size, Copy, _) ->
	<< Copy2:Size/binary, _/binary >> = Copy,
	decompress(Flags, Data, << Window/binary, Copy2/binary >>).
