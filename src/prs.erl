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
	Comp = prealloc_bin:new(byte_size(Data) div 4),
	compress(Data, 0, Comp, [], <<>>).

%% End the compression.
compress(Data, Index, Comp, Flags, Buffer)
		when Index >= byte_size(Data) ->
	Flags2 = [1, 0|Flags],
	NbFlags = length(Flags2),
	Filler = if
		NbFlags > 8 -> 16 - NbFlags;
		true -> 8 - NbFlags
	end,
	{Comp2, Flags3, Buffer2} = copy_flags(Comp, Flags2, Buffer),
	CopyFlags = << << F:1 >> || F <- Flags3 >>,
	<< Comp2/binary, 0:Filler, CopyFlags/bits, Buffer2/binary, 0:16 >>;
%% Not enough data anymore.
compress(Data, Index, Comp, Flags, Buffer)
		when Index >= byte_size(Data) - 3 ->
	copy_byte(Data, Index, Comp, Flags, Buffer);
%% Try compressing.
compress(Data, Index, Comp, Flags, Buffer)
		when Index >= 3 ->
	{Size, Offset} = longest_common(Data, Index),
	copy(Data, Index, Comp, Flags, Buffer, Size, Offset);
%% Not enough data in the window yet.
compress(Data, Index, Comp, Flags, Buffer) ->
	copy_byte(Data, Index, Comp, Flags, Buffer).

copy(Data, Index, Comp, Flags, Buffer, 0, _) ->
	copy_byte(Data, Index, Comp, Flags, Buffer);
copy(Data, Index, Comp, Flags, Buffer, Size, Offset)
		when Size =< 5, Offset > -256 ->
	copy_short(Data, Index, Comp, Flags, Buffer, Size, Offset);
copy(Data, Index, Comp, Flags, Buffer, Size, Offset) ->
	copy_long(Data, Index, Comp, Flags, Buffer, Size, Offset).

copy_byte(Data, Index, Comp, Flags, Buffer) ->
	Byte = binary:at(Data, Index),
	case length(Flags) of
		8 ->
			Flags2 = << << F:1 >> || F <- Flags >>,
			compress(Data, Index + 1,
				<< Comp/binary, Flags2/binary, Buffer/binary >>,
				[1], << Byte >>);
		_ ->
			compress(Data, Index + 1,
				Comp, [1|Flags], << Buffer/binary, Byte >>)
	end.

copy_short(Data, Index, Comp, Flags, Buffer, Size, Offset) ->
	Size2 = Size - 2,
	A = Size2 bsr 1,
	B = Size2 band 1,
	{Comp2, Flags2, Buffer2} = copy_flags(Comp, [B, A, 0, 0|Flags], Buffer),
	compress(Data, Index + Size,
		Comp2, Flags2, << Buffer2/binary, Offset >>).

copy_long(Data, Index, Comp, Flags, Buffer, Size, Offset) ->
	OffsetR = Offset band 16#1f,
	OffsetL = Offset bsr 5,
	{Comp2, Flags2, Buffer2} = copy_flags(Comp, [1, 0|Flags], Buffer),
	if
		Size =< 9 ->
			Size2 = Size - 2,
			compress(Data, Index + Size, Comp2, Flags2,
				<< Buffer2/binary, OffsetR:5, Size2:3, OffsetL:8 >>);
		true ->
			Size2 = Size - 1,
			compress(Data, Index + Size, Comp2, Flags2,
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
longest_common(Data, Index) ->
	WindowOffset = Index - min(Index, 16#1fff),
	longest_common(Data, Index, 3, WindowOffset, 0, 0).

%% End of window.
longest_common(_, Index, _, CurOffset, Size, Offset)
		when CurOffset >= Index ->
	{Size, Offset - Index};
%% We don't have enough data in the buffer anymore.
longest_common(Data, Index, CurSize, _, Size, Offset)
		when byte_size(Data) - Index < CurSize ->
	{Size, Offset - Index};
longest_common(Data, Index, CurSize, CurOffset, Size, Offset) ->
	longest_common_prefix(Data, Index, CurSize, CurOffset, Size, Offset,
		0, Index, CurOffset).

%% We got a segment of maximum size, 255 bytes.
longest_common_prefix(_, Index, _, CurOffset, _, _, 255, _, _) ->
	{255, CurOffset - Index};
longest_common_prefix(Data, Index, CurSize, CurOffset, Size, Offset,
		N, DataOffset, WindowOffset) ->
	DataByte = binary:at(Data, DataOffset),
	WindowByte = binary:at(Data, WindowOffset),
	if
		%% Match, continue unless we're at the end of the data.
		DataByte =:= WindowByte, byte_size(Data) > DataOffset + 1 ->
			longest_common_prefix(Data, Index, CurSize, CurOffset,
				Size, Offset, N + 1, DataOffset + 1, WindowOffset + 1);
		%% No match, but we got a better prefix, save it and try next.
		N >= CurSize ->
			longest_common(Data, Index, N, CurOffset + 1, N, CurOffset);
		%% No match, no better prefix. Try next.
		true ->
			longest_common(Data, Index, CurSize, CurOffset + 1, Size, Offset)
	end.

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
