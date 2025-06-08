% ========================================================================
% SISTEM PAKAR REKOMENDASI POLA HIDUP SEHAT - VERSI DIPERBAIKI
% ========================================================================

% Predikat Dinamis untuk Menyimpan Data Pengguna
:- dynamic profil_pengguna/8.
:- dynamic kondisi_khusus/1.

% ========================================================================
% BASIS PENGETAHUAN: MAKANAN DAN NUTRISI
% ========================================================================

% makanan(Nama, KategoriNutrisi, Kalori, Manfaat, KondisiKhusus)
makanan(nasi_merah, karbohidrat_kompleks, sedang, [energi_berkelanjutan, serat_tinggi], [diabetes, diet]).
makanan(quinoa, protein_karbohidrat, sedang, [protein_lengkap, bebas_gluten], [vegetarian, diabetes]).
makanan(oatmeal, karbohidrat_serat, sedang, [kolesterol_rendah, kenyang_lama], [jantung, diabetes]).
makanan(roti_gandum, karbohidrat_kompleks, sedang, [serat, vitamin_b], [umum]).

makanan(dada_ayam, protein_hewani, sedang, [protein_tinggi, rendah_lemak], [massa_otot]).
makanan(salmon, protein_omega3, tinggi, [omega3, vitamin_d], [jantung, otak]).
makanan(telur, protein_lengkap, sedang, [protein_berkualitas, vitamin_d], [umum]).
makanan(tuna, protein_rendah_lemak, sedang, [protein_tinggi, selenium], [diet, massa_otot]).

makanan(tahu_tempe, protein_nabati, rendah, [protein_nabati, probiotik], [vegetarian]).
makanan(kacang_almond, lemak_protein, tinggi, [lemak_sehat, vitamin_e], [jantung]).
makanan(kacang_merah, protein_serat, sedang, [protein_nabati, zat_besi], [vegetarian, anemia]).

makanan(bayam, sayuran_hijau, rendah, [zat_besi, folat, vitamin_k], [anemia, tulang]).
makanan(brokoli, sayuran_cruciferous, rendah, [vitamin_c, antioksidan], [umum]).
makanan(wortel, sayuran_beta_karoten, rendah, [vitamin_a, serat], [mata]).
makanan(sayur_rebus, sayuran_umum, rendah, [serat, vitamin, mineral], [diet, diabetes]).

makanan(pisang, buah_kalium, sedang, [kalium, vitamin_b6], [hipertensi, olahraga]).
makanan(alpukat, buah_lemak_sehat, tinggi, [lemak_tak_jenuh, kalium], [jantung]).
makanan(buah_beri, buah_antioksidan, rendah, [antioksidan, vitamin_c], [umum]).
makanan(jeruk, buah_vitamin_c, rendah, [vitamin_c, serat], [imunitas]).

makanan(susu_rendah_lemak, susu_kalsium, sedang, [kalsium, protein, vitamin_d], [tulang]).
makanan(yogurt_greek, probiotik, rendah, [probiotik, protein_tinggi], [pencernaan]).
makanan(keju_cottage, protein_susu, sedang, [protein_casein, kalsium], [massa_otot]).

makanan(minyak_zaitun, lemak_sehat, tinggi, [lemak_tak_jenuh, vitamin_e], [jantung]).
makanan(air_putih, hidrasi, nol, [hidrasi_optimal], [umum]).

% ========================================================================
% BASIS PENGETAHUAN: OLAHRAGA DAN AKTIVITAS FISIK
% ========================================================================

% olahraga(Nama, Kategori, Intensitas, Durasi_Menit, Manfaat, Kondisi_Cocok)
olahraga(jalan_kaki, kardio_ringan, rendah, 30, [jantung, sendi_aman], [pemula, lansia, rehabilitasi]).
olahraga(jogging, kardio_sedang, sedang, 20, [stamina, membakar_kalori], [remaja, dewasa]).
olahraga(lari, kardio_tinggi, tinggi, 15, [stamina_tinggi, kalori_maksimal], [atlet, sangat_aktif]).

olahraga(bersepeda, kardio_sendi_aman, sedang, 30, [kaki, kardio, sendi_aman], [semua_usia]).
olahraga(renang, kardio_total, tinggi, 30, [seluruh_tubuh, sendi_aman], [arthritis, cedera]).
olahraga(aqua_aerobik, kardio_air, sedang, 45, [sendi_aman, resistensi], [lansia, arthritis]).

olahraga(angkat_beban, kekuatan, tinggi, 45, [massa_otot, metabolisme], [dewasa, underweight]).
olahraga(push_up, kekuatan_tubuh, sedang, 15, [otot_atas, praktis], [umum]).
olahraga(plank, kekuatan_inti, sedang, 10, [otot_inti, postur], [nyeri_punggung]).

olahraga(yoga, fleksibilitas_mental, rendah, 60, [fleksibilitas, stres, keseimbangan], [stres, kaku]).
olahraga(pilates, kekuatan_fleksibilitas, sedang, 45, [otot_inti, postur, fleksibilitas], [postur_buruk]).
olahraga(tai_chi, keseimbangan_mental, rendah, 30, [keseimbangan, koordinasi, stres], [lansia, keseimbangan]).

olahraga(stretching, fleksibilitas, rendah, 15, [fleksibilitas, relaksasi], [kaku, pemulihan]).
olahraga(meditasi, mental, rendah, 20, [stres, fokus, tidur], [stres, insomnia]).

% ========================================================================
% KLASIFIKASI DAN PERHITUNGAN
% ========================================================================

% Klasifikasi BMI berdasarkan WHO
hitung_bmi(Berat, Tinggi, BMI, Kategori) :-
    BMI is Berat / (Tinggi * Tinggi),
    (   BMI < 18.5 -> Kategori = underweight
    ;   BMI =< 24.9 -> Kategori = normal
    ;   BMI =< 29.9 -> Kategori = overweight
    ;   Kategori = obesitas
    ).

% Kategori Usia yang Lebih Spesifik
kategori_usia(Usia, balita) :- Usia >= 2, Usia =< 5.
kategori_usia(Usia, anak) :- Usia >= 6, Usia =< 12.
kategori_usia(Usia, remaja) :- Usia >= 13, Usia =< 19.
kategori_usia(Usia, dewasa_muda) :- Usia >= 20, Usia =< 35.
kategori_usia(Usia, dewasa) :- Usia >= 36, Usia =< 55.
kategori_usia(Usia, pralansia) :- Usia >= 56, Usia =< 65.
kategori_usia(Usia, lansia) :- Usia > 65.

% Kebutuhan Kalori Harian (estimasi sederhana)
kebutuhan_kalori(Berat, Tinggi, Usia, Aktivitas, JenisKelamin, Kalori) :-
    % BMR menggunakan Mifflin-St Jeor Equation (simplified)
    (   JenisKelamin = pria ->
        BMR is (10 * Berat) + (6.25 * Tinggi * 100) - (5 * Usia) + 5
    ;   BMR is (10 * Berat) + (6.25 * Tinggi * 100) - (5 * Usia) - 161
    ),
    % Faktor aktivitas
    (   Aktivitas = sangat_rendah -> Faktor = 1.2
    ;   Aktivitas = rendah -> Faktor = 1.375
    ;   Aktivitas = sedang -> Faktor = 1.55
    ;   Aktivitas = tinggi -> Faktor = 1.725
    ;   Faktor = 1.9
    ),
    Kalori is BMR * Faktor.

% ========================================================================
% ATURAN REKOMENDASI CERDAS
% ========================================================================

% Rekomendasi Berdasarkan Prioritas Kesehatan
rekomendasi_makanan_prioritas(Makanan, Alasan) :-
    kondisi_khusus(Kondisi),
    makanan(Makanan, _, _, Manfaat, KondisiKhusus),
    (member(Kondisi, KondisiKhusus) ; member(umum, KondisiKhusus)),
    kondisi_manfaat(Kondisi, Manfaat, Alasan).

% Mapping kondisi kesehatan dengan manfaat yang dibutuhkan
kondisi_manfaat(diabetes, Manfaat, 'Membantu mengontrol gula darah dan memberikan energi stabil') :-
    (member(energi_berkelanjutan, Manfaat) ; member(serat_tinggi, Manfaat)).
kondisi_manfaat(jantung, Manfaat, 'Mendukung kesehatan jantung dan mengurangi risiko penyakit kardiovaskular') :-
    (member(omega3, Manfaat) ; member(lemak_sehat, Manfaat) ; member(kolesterol_rendah, Manfaat)).
kondisi_manfaat(hipertensi, Manfaat, 'Membantu mengontrol tekanan darah') :-
    member(kalium, Manfaat).
kondisi_manfaat(anemia, Manfaat, 'Meningkatkan kadar zat besi dan mencegah anemia') :-
    (member(zat_besi, Manfaat) ; member(folat, Manfaat)).
kondisi_manfaat(osteoporosis, Manfaat, 'Memperkuat tulang dan mencegah keropos tulang') :-
    (member(kalsium, Manfaat) ; member(vitamin_d, Manfaat)).
kondisi_manfaat(vegetarian, Manfaat, 'Sumber protein nabati yang lengkap untuk vegetarian') :-
    member(protein_nabati, Manfaat).

% Rekomendasi berdasarkan tujuan berat badan
rekomendasi_berat_badan(underweight, Makanan, 'Membantu penambahan berat badan sehat dengan kalori dan nutrisi padat') :-
    makanan(Makanan, _, tinggi, _, _).
rekomendasi_berat_badan(normal, Makanan, 'Mempertahankan berat badan ideal dengan nutrisi seimbang') :-
    makanan(Makanan, _, sedang, _, _).
rekomendasi_berat_badan(overweight, Makanan, 'Mendukung penurunan berat badan dengan kalori rendah namun nutrisi tinggi') :-
    makanan(Makanan, _, rendah, _, _).
rekomendasi_berat_badan(obesitas, Makanan, 'Membantu program penurunan berat badan dengan makanan rendah kalori dan tinggi serat') :-
    makanan(Makanan, _, rendah, Manfaat, _),
    (member(serat_tinggi, Manfaat) ; member(protein_tinggi, Manfaat)).

% Rekomendasi olahraga berdasarkan kondisi dan tujuan
rekomendasi_olahraga_kondisi(Kondisi, Olahraga, Alasan) :-
    olahraga(Olahraga, _, _, _, Manfaat, KondisiCocok),
    member(Kondisi, KondisiCocok),
    format(atom(Alasan), 'Cocok untuk kondisi ~w dengan manfaat: ~w', [Kondisi, Manfaat]).

% Rekomendasi olahraga berdasarkan usia
rekomendasi_olahraga_usia(KategoriUsia, Olahraga, Alasan) :-
    kategori_usia_olahraga(KategoriUsia, Olahraga, Alasan).

kategori_usia_olahraga(balita, bermain_aktif, 'Aktivitas bermain untuk perkembangan motorik dan koordinasi').
kategori_usia_olahraga(anak, bersepeda, 'Melatih keseimbangan dan koordinasi, serta menyenangkan').
kategori_usia_olahraga(anak, renang, 'Olahraga menyeluruh yang aman untuk pertumbuhan').
kategori_usia_olahraga(remaja, jogging, 'Membangun stamina dan kesehatan kardiovaskular').
kategori_usia_olahraga(remaja, angkat_beban, 'Membangun massa otot (dengan pengawasan yang tepat)').
kategori_usia_olahraga(dewasa_muda, renang, 'Olahraga komprehensif untuk kebugaran optimal').
kategori_usia_olahraga(dewasa_muda, angkat_beban, 'Mempertahankan dan membangun massa otot').
kategori_usia_olahraga(dewasa, yoga, 'Mengurangi stres dan meningkatkan fleksibilitas').
kategori_usia_olahraga(dewasa, pilates, 'Memperkuat otot inti dan memperbaiki postur').
kategori_usia_olahraga(pralansia, tai_chi, 'Meningkatkan keseimbangan dan koordinasi').
kategori_usia_olahraga(pralansia, jalan_kaki, 'Aktivitas kardio yang aman dan dapat dilakukan setiap hari').
kategori_usia_olahraga(lansia, tai_chi, 'Mencegah jatuh dan meningkatkan keseimbangan').
kategori_usia_olahraga(lansia, stretching, 'Mempertahankan fleksibilitas dan mengurangi kekakuan').



mulai_sistem :-
    write('========================================================='), nl,
    write('    SISTEM PAKAR REKOMENDASI POLA HIDUP SEHAT'), nl,
    write('========================================================='), nl,
    write('Sistem ini akan memberikan rekomendasi makanan dan olahraga'), nl,
    write('berdasarkan profil kesehatan dan tujuan Anda.'), nl, nl,

    retractall(profil_pengguna(_, _, _, _, _, _, _, _)),
    retractall(kondisi_khusus(_)),

    kumpulkan_data_pengguna(Usia, Berat, Tinggi, JenisKelamin, Aktivitas, PolaMakan, Stres, Tidur),
    kumpulkan_kondisi_khusus,

    assertz(profil_pengguna(Usia, Berat, Tinggi, JenisKelamin, Aktivitas, PolaMakan, Stres, Tidur)),

    tampilkan_analisis_lengkap.

kumpulkan_data_pengguna(Usia, Berat, Tinggi, JenisKelamin, Aktivitas, PolaMakan, Stres, Tidur) :-
    % Usia
    repeat,
    write('Masukkan usia Anda (tahun): '),
    read(Usia),
    (number(Usia), Usia > 0, Usia < 120 -> true ;
     (write('Usia tidak valid! Masukkan angka 1-119.'), nl, fail)),
    !,

    % Jenis Kelamin
    repeat,
    write('Jenis kelamin (pria/wanita): '),
    read(JenisKelamin),
    member(JenisKelamin, [pria, wanita]),
    !,

    % Berat dan Tinggi
    repeat,
    write('Berat badan (kg): '),
    read(Berat),
    (number(Berat), Berat > 0 -> true ;
     (write('Berat badan tidak valid!'), nl, fail)),
    !,

    repeat,
    write('Tinggi badan (meter, contoh: 1.70): '),
    read(Tinggi),
    (number(Tinggi), Tinggi > 0.5, Tinggi < 3.0 -> true ;
     (write('Tinggi badan tidak valid!'), nl, fail)),
    !,

    % Tingkat Aktivitas
    write('Tingkat aktivitas fisik:'), nl,
    write('1. Sangat rendah (tidak pernah olahraga)'), nl,
    write('2. Rendah (olahraga 1-2x/minggu)'), nl,
    write('3. Sedang (olahraga 3-4x/minggu)'), nl,
    write('4. Tinggi (olahraga 5-6x/minggu)'), nl,
    write('5. Sangat tinggi (olahraga setiap hari)'), nl,
    repeat,
    write('Pilihan (1-5): '),
    read(PilAktivitas),
    member(PilAktivitas, [1,2,3,4,5]),
    konversi_aktivitas(PilAktivitas, Aktivitas),
    !,

    % Pola Makan
    write('Pola makan saat ini:'), nl,
    write('1. Teratur dan seimbang'), nl,
    write('2. Tidak teratur'), nl,
    write('3. Sering makan berlebihan'), nl,
    write('4. Kurang nutrisi'), nl,
    write('5. Tinggi gula dan lemak'), nl,
    repeat,
    write('Pilihan (1-5): '),
    read(PilMakan),
    member(PilMakan, [1,2,3,4,5]),
    konversi_pola_makan(PilMakan, PolaMakan),
    !,

    % Tingkat Stres
    repeat,
    write('Tingkat stres (rendah/sedang/tinggi): '),
    read(Stres),
    member(Stres, [rendah, sedang, tinggi]),
    !,

    % Kualitas Tidur
    repeat,
    write('Rata-rata jam tidur per malam: '),
    read(JamTidur),
    (number(JamTidur) ->
        (JamTidur < 6 -> Tidur = kurang_sekali ;
         JamTidur < 7 -> Tidur = kurang ;
         JamTidur =< 9 -> Tidur = cukup ;
         Tidur = terlalu_banyak)
    ; Tidur = tidak_tahu),
    !.

konversi_aktivitas(1, sangat_rendah).
konversi_aktivitas(2, rendah).
konversi_aktivitas(3, sedang).
konversi_aktivitas(4, tinggi).
konversi_aktivitas(5, sangat_tinggi).

konversi_pola_makan(1, teratur_seimbang).
konversi_pola_makan(2, tidak_teratur).
konversi_pola_makan(3, berlebihan).
konversi_pola_makan(4, kurang_nutrisi).
konversi_pola_makan(5, tinggi_gula_lemak).

kumpulkan_kondisi_khusus :-
    write('Apakah Anda memiliki kondisi kesehatan khusus?'), nl,
    write('Ketik kondisi yang sesuai (pisahkan dengan spasi jika lebih dari satu):'), nl,
    write('- diabetes'), nl,
    write('- jantung (penyakit jantung)'), nl,
    write('- hipertensi'), nl,
    write('- anemia'), nl,
    write('- osteoporosis'), nl,
    write('- arthritis'), nl,
    write('- vegetarian'), nl,
    write('- tidak_ada'), nl,
    write('Kondisi: '),
    read_line_to_string(user_input, Input),
    split_string(Input, ' ,.', ' ,.', KondisiList), % Memperbaiki pemisah
    forall(member(KondisiStr, KondisiList),
           (atom_string(KondisiAtom, KondisiStr),
            (KondisiAtom \= tidak_ada, KondisiAtom \= '' -> assertz(kondisi_khusus(KondisiAtom)) ; true))).

% ========================================================================
% ANALISIS DAN REKOMENDASI KOMPREHENSIF
% ========================================================================

tampilkan_analisis_lengkap :-
    profil_pengguna(Usia, Berat, Tinggi, JenisKelamin, Aktivitas, PolaMakan, Stres, Tidur),

    % Analisis BMI
    hitung_bmi(Berat, Tinggi, BMI, StatusBerat),
    kategori_usia(Usia, KategoriUsia),
    kebutuhan_kalori(Berat, Tinggi, Usia, Aktivitas, JenisKelamin, Kalori),

    % Tampilkan profil
    nl,
    write('=================== ANALISIS PROFIL ANDA ==================='), nl,
    format('Usia: ~w tahun (~w)~n', [Usia, KategoriUsia]),
    format('BMI: ~2f (~w)~n', [BMI, StatusBerat]),
    format('Kebutuhan Kalori Harian: ~0f kalori~n', [Kalori]),
    format('Tingkat Aktivitas: ~w~n', [Aktivitas]),
    format('Pola Makan: ~w~n', [PolaMakan]),
    format('Tingkat Stres: ~w~n', [Stres]),
    format('Kualitas Tidur: ~w~n', [Tidur]),

    write('Kondisi Khusus: '),
    (kondisi_khusus(K) ->
        forall(kondisi_khusus(Kond), format('~w ', [Kond])) ;
        write('tidak ada')),
    nl, nl,

    % Rekomendasi berdasarkan prioritas
    write('================= REKOMENDASI MAKANAN ================='), nl,
    tampilkan_rekomendasi_makanan(StatusBerat),

    nl,
    write('================= REKOMENDASI OLAHRAGA ================'), nl,
    tampilkan_rekomendasi_olahraga(KategoriUsia, StatusBerat),

    nl,
    write('================== TIPS KESEHATAN ===================='), nl,
    tampilkan_tips_personal(StatusBerat, PolaMakan, Stres, Tidur),

    nl,
    write('===================== PERINGATAN ====================='), nl,
    write('* Konsultasikan dengan dokter sebelum memulai program baru.'), nl,
    write('* Lakukan perubahan secara bertahap.'), nl,
    write('* Monitor respons tubuh Anda.'), nl,
    write('======================================================'), nl.

tampilkan_rekomendasi_makanan(StatusBerat) :-
    write('Makanan yang direkomendasikan:'), nl,

    % Prioritas 1: Kondisi kesehatan khusus
    (kondisi_khusus(_) ->
        (write('→ Berdasarkan kondisi kesehatan:'), nl,
         % Menggunakan setof untuk menghindari duplikasi
         setof(Makanan-Alasan, rekomendasi_makanan_prioritas(Makanan, Alasan), RekomendasiPrioritas),
         forall(member(M-A, RekomendasiPrioritas), format('  • ~w - ~w~n', [M, A]))), nl
    ; true),

    % Prioritas 2: Status berat badan
    write('→ Berdasarkan target berat badan:'), nl,
    setof(Makanan-Alasan, rekomendasi_berat_badan(StatusBerat, Makanan, Alasan), RekomendasiBerat),
    forall(member(M-A, RekomendasiBerat), format('  • ~w - ~w~n', [M, A])),

    % Rekomendasi umum
    nl,
    write('→ Makanan wajib untuk semua:'), nl,
    write('  • Air putih - Minimal 8 gelas per hari untuk hidrasi optimal.'), nl,
    write('  • Sayuran hijau - Sumber vitamin, mineral, dan serat.'), nl,
    write('  • Buah-buahan - Antioksidan dan vitamin C untuk imunitas.'), nl.

tampilkan_rekomendasi_olahraga(KategoriUsia, StatusBerat) :-
    write('Program olahraga yang sesuai:'), nl,

    % Berdasarkan usia
    write('→ Sesuai usia Anda:'), nl,
    setof(Olahraga-Alasan, rekomendasi_olahraga_usia(KategoriUsia, Olahraga, Alasan), RekomendasiUsia),
    forall(member(O-A, RekomendasiUsia), format('  • ~w - ~w~n', [O, A])),

    % Berdasarkan kondisi khusus
    (kondisi_khusus(_) ->
        (nl, write('→ Sesuai kondisi kesehatan:'), nl,
         setof(Olahraga-Alasan, Kondisi^(kondisi_khusus(Kondisi), rekomendasi_olahraga_kondisi(Kondisi, Olahraga, Alasan)), RekomendasiKondisi),
         forall(member(O-A, RekomendasiKondisi), format('  • ~w - ~w~n', [O, A])))
    ; true),

    % Tips durasi dan frekuensi
    nl,
    write('→ Panduan umum:'), nl,
    panduan_olahraga_umum(StatusBerat).

panduan_olahraga_umum(underweight) :-
    write('  • Fokus pada latihan kekuatan 3-4x/minggu.'), nl,
    write('  • Kardio ringan 2-3x/minggu.'), nl,
    write('  • Istirahat cukup untuk recovery.').
panduan_olahraga_umum(normal) :-
    write('  • Kombinasi kardio dan kekuatan 4-5x/minggu.'), nl,
    write('  • Variasi jenis olahraga untuk mencegah bosan.'), nl,
    write('  • Maintenance program yang konsisten.').
panduan_olahraga_umum(overweight) :-
    write('  • Kardio 4-5x/minggu, 30-45 menit.'), nl,
    write('  • Latihan kekuatan 2-3x/minggu.'), nl,
    write('  • Mulai bertahap, tingkatkan intensitas perlahan.').
panduan_olahraga_umum(obesitas) :-
    write('  • Mulai dengan jalan kaki 20-30 menit setiap hari.'), nl,
    write('  • Tambahkan aktivitas air jika memungkinkan.'), nl,
    write('  • Konsultasi dengan profesional untuk program yang aman.').

tampilkan_tips_personal(StatusBerat, PolaMakan, Stres, Tidur) :-
    % Tips berdasarkan pola makan
    tips_pola_makan(PolaMakan), nl,

    % Tips berdasarkan stres
    tips_manajemen_stres(Stres), nl,

    % Tips berdasarkan kualitas tidur
    tips_kualitas_tidur(Tidur), nl,

    % Tips khusus berdasarkan BMI
    tips_berat_badan(StatusBerat), nl.

tips_pola_makan(tidak_teratur) :-
    write('📝 Pola Makan:'), nl,
    write('  • Buat jadwal makan yang konsisten.'), nl,
    write('  • Siapkan snack sehat untuk darurat.'), nl,
    write('  • Gunakan pengingat untuk waktu makan.').
tips_pola_makan(berlebihan) :-
    write('📝 Pola Makan:'), nl,
    write('  • Gunakan piring yang lebih kecil.'), nl,
    write('  • Makan perlahan dan nikmati makanan.'), nl,
    write('  • Minum air sebelum makan.').
tips_pola_makan(kurang_nutrisi) :-
    write('📝 Pola Makan:'), nl,
    write('  • Konsultasi dengan ahli gizi.'), nl,
    write('  • Pertimbangkan suplemen jika perlu.'), nl,
    write('  • Fokus pada makanan padat nutrisi.').
tips_pola_makan(_) :- % Mencakup teratur_seimbang dan tinggi_gula_lemak
    write('📝 Pola Makan:'), nl,
    write('  • Pertahankan kebiasaan makan yang baik dan teratur.'), nl,
    write('  • Jika pola makan tinggi gula/lemak, kurangi secara bertahap.').

tips_manajemen_stres(tinggi) :-
    write('🧘 Manajemen Stres:'), nl,
    write('  • Luangkan waktu untuk relaksasi setiap hari.'), nl,
    write('  • Coba teknik pernapasan dalam atau meditasi.'), nl,
    write('  • Pertimbangkan konseling jika diperlukan.').
tips_manajemen_stres(sedang) :-
    write('🧘 Manajemen Stres:'), nl,
    write('  • Lakukan hobi yang menyenangkan.'), nl,
    write('  • Jaga keseimbangan antara pekerjaan dan kehidupan pribadi.'). % # DIPERBAIKI: Aturan ditutup dengan titik.
tips_manajemen_stres(rendah) :- % 
    write('🧘 Manajemen Stres:'), nl,
    write('  • Sangat baik! Pertahankan tingkat stres yang rendah.').

tips_kualitas_tidur(kurang_sekali) :-
    write('😴 Kualitas Tidur:'), nl,
    write('  • Kualitas tidur Anda sangat kurang. Ini berisiko bagi kesehatan.'), nl,
    write('  • Prioritaskan untuk tidur 7-9 jam setiap malam.'), nl,
    write('  • Ciptakan rutinitas tidur yang menenangkan, hindari kafein dan gadget sebelum tidur.').
tips_kualitas_tidur(kurang) :-
    write('😴 Kualitas Tidur:'), nl,
    write('  • Usahakan untuk menambah durasi tidur Anda.'), nl,
    write('  • Pastikan lingkungan tidur Anda gelap, sejuk, dan tenang.').
tips_kualitas_tidur(cukup) :-
    write('😴 Kualitas Tidur:'), nl,
    write('  • Kualitas tidur Anda sudah baik. Pertahankan!.').
tips_kualitas_tidur(terlalu_banyak) :-
    write('😴 Kualitas Tidur:'), nl,
    write('  • Tidur berlebihan terkadang bisa menjadi indikator masalah kesehatan.'), nl,
    write('  • Coba atur alarm untuk bangun pada jam yang konsisten.').
tips_kualitas_tidur(_) :- !. % Jika 'tidak_tahu', tidak menampilkan apa-apa.

tips_berat_badan(underweight) :-
    write('⚖️ Tips Berat Badan (Underweight):'), nl,
    write('  • Fokus pada surplus kalori sehat dari makanan padat nutrisi, bukan junk food.'), nl,
    write('  • Tingkatkan frekuensi makan menjadi 5-6 kali porsi kecil sehari.').
tips_berat_badan(normal) :-
    write('⚖️ Tips Berat Badan (Normal):'), nl,
    write('  • Selamat! Berat badan Anda sudah ideal.'), nl,
    write('  • Pertahankan pola makan seimbang dan aktivitas fisik rutin.').
tips_berat_badan(overweight) :-
    write('⚖️ Tips Berat Badan (Overweight):'), nl,
    write('  • Ciptakan defisit kalori yang wajar (kurangi ~500 kalori per hari).'), nl,
    write('  • Perhatikan ukuran porsi dan perbanyak konsumsi serat (sayur & buah).').
tips_berat_badan(obesitas) :-
    write('⚖️ Tips Berat Badan (Obesitas):'), nl,
    write('  • Sangat disarankan untuk berkonsultasi dengan dokter atau ahli gizi.'), nl,
    write('  • Fokus pada perubahan gaya hidup jangka panjang yang berkelanjutan.').
