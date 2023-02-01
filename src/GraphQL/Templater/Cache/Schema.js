import * as LZString from 'lz-string';

export function compress(string) {
  return LZString.compressToUTF16(string);
}

export function decompress(str) {
  return LZString.decompressFromUTF16(str);

}

const encoding = 'deflate';