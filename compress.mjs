// compresses fable's sometimes nonideal output with SWC
import fg from "fast-glob";
import { transformFile } from "@swc/core";
import { writeFile } from "fs/promises";

for await (const file of fg.stream("dist/**/*.js"))
  await writeFile(
    file,
    (
      await transformFile(file, {
        minify: false,
        jsc: {
          minify: {
            compress: true,
            mangle: false,
          },
          target: "es2022",
        },
      })
    ).code
  );