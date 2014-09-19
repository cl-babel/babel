(in-package #:babel-encodings)

(define-character-encoding :ebcdic-international
    "An 8-bit, fixed-width character encoding from IBM.")

(defparameter +ebcdic-int-to-unicode+ 
  #(0 1 2 3 156 9 134 127 151 141 142 11 12 13 14 15 16 17 18 19 157 133 8 135 24
    25 146 143 28 29 30 31 128 129 130 65535 132 10 23 27 65535 65535 138 139
    65535 5 6 7 65535 65535 22 65535 148 149 150 4 65535 65535 65535 155 20 21
    65535 26 32 65535 65535 65535 65535 65535 65535 65535 65535 65535 91 46 60 40
    43 33 38 65535 65535 65535 65535 65535 65535 65535 65535 65535 93 164 42 41 59
    172 45 47 65535 65535 65535 65535 65535 65535 65535 65535 124 44 37 95 62 63
    65535 65535 65535 65535 65535 65535 1102 1072 1073 96 58 35 64 39 61 34 1094
    97 98 99 100 101 102 103 104 105 1076 1077 1092 1075 1093 1080 1081 106 107
    108 109 110 111 112 113 114 1082 1083 1084 1085 1086 1087 1103 126 115 116 117
    118 119 120 121 122 1088 1089 1090 1091 1078 1074 1100 1099 1079 1096 1101
    1097 1095 1098 1070 1040 1041 1062 1044 1045 1060 1043 123 65 66 67 68 69 70
    71 72 73 1061 1048 1049 1050 1051 1052 125 74 75 76 77 78 79 80 81 82 1053
    1054 1055 1071 1056 1057 92 65535 83 84 85 86 87 88 89 90 1058 1059 1046 1042
    1068 1067 48 49 50 51 52 53 54 55 56 57 1047 1064 1069 1065 1063 159))

(defparameter +unicode-upto-ac-ebcdic-int+
  #(0 1 2 3 55 45 46 47 22 5 37 11 12 13 14 15 16 17 18 19 60 61 50 38 24 25 63
    39 28 29 30 31 64 79 127 123 0 108 80 125 77 93 92 78 107 96 75 97 240 241
    242 243 244 245 246 247 248 249 122 94 76 126 110 111 124 193 194 195 196 197
    198 199 200 201 209 210 211 212 213 214 215 216 217 226 227 228 229 230 231
    232 233 74 224 90 0 109 121 129 130 131 132 133 134 135 136 137 145 146 147
    148 149 150 151 152 153 162 163 164 165 166 167 168 169 192 106 208 161 7 32
    33 34 0 36 21 6 23 0 0 42 43 0 9 10 27 0 0 26 0 52 53 54 8 0 0 0 59 4 20 0
    255 0 0 0 0 91 0 0 0 0 0 0 0 95))

(defparameter +unicode-0410-0450-ebcdic-int+
  #(185 186 237 191 188 189 236 250 203 204 205 206 207 218 219 220 222 223 234
    235 190 202 187 254 251 253 0 239 238 252 184 221 119 120 175 141 138 139 174
    178 143 144 154 155 156 157 158 159 170 171 172 173 140 142 128 182 179 181
    183 177 176 180 118 160))

(define-unibyte-decoder :ebcdic-international (octet)
  (svref +ebcdic-int-to-unicode+ (the ub8 octet)))

(define-unibyte-encoder :ebcdic-international (code)
  (let ((result (cond
                  ((<= code 172) (svref +unicode-upto-ac-ebcdic-int+ code))
                  ((<= #x0410 code #x0450) (svref +unicode-0410-0450-ebcdic-int+
                                                 (- code #x0410))))))
    (prog1 result
      (when (and (zerop result) (plusp code))
        (handle-error)))))
