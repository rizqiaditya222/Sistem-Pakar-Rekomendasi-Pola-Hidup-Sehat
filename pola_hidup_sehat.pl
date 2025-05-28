
% Predikat Dinamis
:- dynamic usia/1.
:- dynamic berat_badan/1.
:- dynamic aktivitas/1.
:- dynamic pola_makan/1.
:- dynamic tingkat_stres/1.
:- dynamic kondisi_kesehatan/1.
:- dynamic preferensi_makanan/1.
:- dynamic preferensi_olahraga/1.
:- dynamic waktu_tidur/1.

% Fakta: Jenis makanan
makanan(sayur_rebus).
makanan(dada_ayam).
makanan(oatmeal).
makanan(salmon).
makanan(buah_buahan).
makanan(nasi_merah).
makanan(tahu_tempe).
makanan(jus_sayur).
makanan(air_putih).
makanan(roti_gandum).
makanan(alpukat).
makanan(telur).
makanan(yogurt_rendah_lemak).

% Fakta: Jenis olahraga
olahraga(jogging).
olahraga(yoga).
olahraga(angkat_beban).
olahraga(bersepeda).
olahraga(renang).
olahraga(plank).
olahraga(jalan_kaki).
olahraga(meditasi).
olahraga(pilates).
olahraga(latihan_interval_intensitas_tinggi). % HIIT

% Fakta: Pola makan 
pola_makan_jenis(teratur_seimbang).
pola_makan_jenis(tidak_teratur).
pola_makan_jenis(berlebihan).
pola_makan_jenis(kurang_nutrisi).
pola_makan_jenis(tinggi_gula_dan_lemak).

% Fakta: Aktivitas
aktivitas_jenis(pasif).
aktivitas_jenis(sedang).
aktivitas_jenis(aktif).

% Fakta: Tingkat Stres 
tingkat_stres_jenis(rendah).
tingkat_stres_jenis(sedang).
tingkat_stres_jenis(tinggi).

% Fakta: Kondisi Kesehatan 
kondisi_kesehatan_jenis(tidak_ada).
kondisi_kesehatan_jenis(diabetes).
kondisi_kesehatan_jenis(penyakit_jantung).
kondisi_kesehatan_jenis(tekanan_darah_tinggi).
kondisi_kesehatan_jenis(kolesterol_tinggi).
kondisi_kesehatan_jenis(gangguan_pencernaan).

% Perhitungan BMI dan klasifikasi berat
klasifikasi_berat(Berat, Tinggi, underweight) :-
    BMI is Berat / (Tinggi * Tinggi),
    BMI < 18.5.

klasifikasi_berat(Berat, Tinggi, ideal) :-
    BMI is Berat / (Tinggi * Tinggi),
    BMI >= 18.5,
    BMI =< 24.9.

klasifikasi_berat(Berat, Tinggi, overweight) :-
    BMI is Berat / (Tinggi * Tinggi),
    BMI > 24.9,
    BMI =< 29.9. % Tambahan kategori overweight sampai 29.9

klasifikasi_berat(Berat, Tinggi, obesitas) :-
    BMI is Berat / (Tinggi * Tinggi),
    BMI > 29.9. % Kategori baru: obesitas

% Aturan rekomendasi makanan 
rekomendasi_makanan(salmon, 'Sumber protein dan omega-3 yang bagus untuk underweight atau aktivitas aktif.') :-
    berat_badan(underweight),
    aktivitas(aktif).

rekomendasi_makanan(dada_ayam, 'Protein tanpa lemak untuk membangun otot, baik bagi underweight dan aktivitas sedang.') :-
    berat_badan(underweight),
    aktivitas(sedang).

rekomendasi_makanan(oatmeal, 'Karbohidrat kompleks yang memberikan energi stabil, ideal untuk pola makan teratur.') :-
    berat_badan(ideal),
    pola_makan(teratur_seimbang).

rekomendasi_makanan(nasi_merah, 'Karbohidrat kompleks dengan serat tinggi, baik untuk kontrol berat badan overweight.') :-
    berat_badan(overweight),
    aktivitas(aktif).

rekomendasi_makanan(sayur_rebus, 'Rendah kalori, tinggi serat dan vitamin, sangat baik untuk mengurangi gula dan lemak.') :-
    berat_badan(overweight),
    pola_makan(tinggi_gula_dan_lemak).

rekomendasi_makanan(jus_sayur, 'Cara mudah mendapatkan nutrisi dari sayuran, membantu detoksifikasi dari gula dan lemak.') :-
    pola_makan(tinggi_gula_dan_lemak).

rekomendasi_makanan(air_putih, 'Penting untuk hidrasi dan metabolisme, membantu mengurangi keinginan akan minuman manis.') :-
    pola_makan(tinggi_gula_dan_lemak).

rekomendasi_makanan(buah_buahan, 'Sumber vitamin dan serat alami, baik untuk melengkapi nutrisi jika pola makan tidak teratur.') :-
    pola_makan(tidak_teratur).

rekomendasi_makanan(tahu_tempe, 'Sumber protein nabati yang baik, cocok untuk vegetarian atau variasi protein.') :-
    preferensi_makanan(vegetarian).

rekomendasi_makanan(alpukat, 'Lemak sehat yang baik untuk jantung, direkomendasikan untuk kolesterol tinggi atau kurang nutrisi.') :-
    (kondisi_kesehatan(kolesterol_tinggi) ; pola_makan(kurang_nutrisi)).

rekomendasi_makanan(telur, 'Protein lengkap dan nutrisi penting, baik untuk underweight atau pemulihan energi.') :-
    berat_badan(underweight) ; aktivitas(aktif).

rekomendasi_makanan(yogurt_rendah_lemak, 'Probiotik untuk pencernaan, direkomendasikan jika ada gangguan pencernaan atau diet.') :-
    kondisi_kesehatan(gangguan_pencernaan).

rekomendasi_makanan(oatmeal, 'Sarapan sehat yang menenangkan, baik saat stres.') :-
    tingkat_stres(tinggi).

rekomendasi_makanan(sayur_rebus, 'Mudah dicerna dan kaya nutrisi, ideal untuk kondisi pencernaan sensitif.') :-
    kondisi_kesehatan(gangguan_pencernaan).

rekomendasi_makanan(nasi_merah, 'Pilihan karbohidrat dengan indeks glikemik lebih rendah untuk penderita diabetes.') :-
    kondisi_kesehatan(diabetes).

% Aturan rekomendasi olahraga 
rekomendasi_olahraga(jogging, 'Baik untuk membakar kalori dan meningkatkan kardio, cocok untuk overweight dan pasif.') :-
    berat_badan(overweight),
    aktivitas(pasif),
    \+ kondisi_kesehatan(penyakit_jantung). % Hindari jika ada penyakit jantung

rekomendasi_olahraga(yoga, 'Meningkatkan fleksibilitas dan mengurangi stres, sangat baik untuk usia lanjut dan stres tinggi.') :-
    usia(U), U >= 50 ; tingkat_stres(tinggi).

rekomendasi_olahraga(angkat_beban, 'Membangun massa otot dan kekuatan, ideal untuk underweight dan aktivitas aktif.') :-
    berat_badan(underweight),
    aktivitas(aktif).

rekomendasi_olahraga(renang, 'Olahraga kardio seluruh tubuh yang rendah dampak, cocok untuk overweight dan sendi sensitif.') :-
    berat_badan(overweight),
    aktivitas(sedang).

rekomendasi_olahraga(jalan_kaki, 'Mudah dilakukan, efektif untuk pemula dan mengurangi dampak pola makan buruk.') :-
    aktivitas(pasif),
    pola_makan(tinggi_gula_dan_lemak).

rekomendasi_olahraga(bersepeda, 'Kardio yang menyenangkan dan efektif, baik untuk mempertahankan berat badan ideal.') :-
    berat_badan(ideal),
    aktivitas(aktif).

rekomendasi_olahraga(plank, 'Membangun kekuatan inti, cocok untuk menjaga kebugaran saat berat badan ideal.') :-
    berat_badan(ideal),
    pola_makan(teratur_seimbang).

rekomendasi_olahraga(meditasi, 'Sangat efektif untuk mengurangi stres dan meningkatkan kualitas tidur.') :-
    tingkat_stres(tinggi) ; waktu_tidur(kurang).

rekomendasi_olahraga(pilates, 'Meningkatkan kekuatan, fleksibilitas, dan keseimbangan, baik untuk semua usia dan kondisi.') :-
    preferensi_olahraga(pilates) ; tingkat_stres(sedang).

rekomendasi_olahraga(latihan_interval_intensitas_tinggi, 'Membakar kalori secara efisien dalam waktu singkat, cocok untuk aktivitas aktif dan target penurunan berat badan cepat.') :-
    aktivitas(aktif),
    (berat_badan(overweight) ; berat_badan(obesitas)),
    \+ kondisi_kesehatan(penyakit_jantung),
    \+ usia(U), U >= 50.

% Interaksi pengguna
mulai :-
    write('Selamat datang di Sistem Pakar Kondisi Badan yang Lebih Akurat!'), nl,
    retractall(usia(_)),
    retractall(berat_badan(_)),
    retractall(aktivitas(_)),
    retractall(pola_makan(_)),
    retractall(tingkat_stres(_)),
    retractall(kondisi_kesehatan(_)),
    retractall(preferensi_makanan(_)),
    retractall(preferensi_olahraga(_)),
    retractall(waktu_tidur(_)),

    tanya_usia,
    tanya_berat_dan_tinggi,
    tanya_aktivitas,
    tanya_pola_makan,
    tanya_tingkat_stres,
    tanya_kondisi_kesehatan,
    tanya_preferensi_makanan,
    tanya_preferensi_olahraga,
    tanya_waktu_tidur,
    tampilkan_rekomendasi.

tanya_usia :-
    write('Masukkan usia Anda (dalam tahun): '),
    read(U),
    assertz(usia(U)).

tanya_berat_dan_tinggi :-
    write('Masukkan berat badan Anda (kg): '),
    read(Berat),
    write('Masukkan tinggi badan Anda (dalam meter, contoh 1.65): '),
    read(Tinggi),
    klasifikasi_berat(Berat, Tinggi, Kategori),
    assertz(berat_badan(Kategori)),
    format('BMI Anda diklasifikasikan sebagai: ~w~n', [Kategori]).

tanya_aktivitas :-
    write('Masukkan tingkat aktivitas Anda (pasif/sedang/aktif): '),
    read(A),
    (aktivitas_jenis(A) -> assertz(aktivitas(A)) ; (write('Input tidak valid, gunakan pasif/sedang/aktif.'), nl, tanya_aktivitas)).

tanya_pola_makan :-
    write('Masukkan pola makan Anda (teratur_seimbang/tidak_teratur/berlebihan/kurang_nutrisi/tinggi_gula_dan_lemak): '),
    read(Pola),
    (pola_makan_jenis(Pola) -> assertz(pola_makan(Pola)) ; (write('Input tidak valid.'), nl, tanya_pola_makan)).

tanya_tingkat_stres :-
    write('Bagaimana tingkat stres Anda (rendah/sedang/tinggi)? '),
    read(Stres),
    (tingkat_stres_jenis(Stres) -> assertz(tingkat_stres(Stres)) ; (write('Input tidak valid, gunakan rendah/sedang/tinggi.'), nl, tanya_tingkat_stres)).

tanya_kondisi_kesehatan :-
    write('Apakah Anda memiliki kondisi kesehatan tertentu (tidak_ada/diabetes/penyakit_jantung/tekanan_darah_tinggi/kolesterol_tinggi/gangguan_pencernaan)? '),
    read(Kondisi),
    (kondisi_kesehatan_jenis(Kondisi) -> assertz(kondisi_kesehatan(Kondisi)) ; (write('Input tidak valid.'), nl, tanya_kondisi_kesehatan)).

tanya_preferensi_makanan :-
    write('Apakah ada preferensi makanan khusus (vegetarian/non_vegetarian/tidak_ada)? '),
    read(Preferensi),
    (member(Preferensi, [vegetarian, non_vegetarian, tidak_ada]) -> assertz(preferensi_makanan(Preferensi)) ; (write('Input tidak valid.'), nl, tanya_preferensi_makanan)).

tanya_preferensi_olahraga :-
    write('Apakah ada preferensi olahraga khusus (yoga/pilates/angkat_beban/tidak_ada)? '),
    read(Preferensi),
    (member(Preferensi, [yoga, pilates, angkat_beban, tidak_ada]) -> assertz(preferensi_olahraga(Preferensi)) ; (write('Input tidak valid.'), nl, tanya_preferensi_olahraga)).

tanya_waktu_tidur :-
    write('Bagaimana kualitas tidur Anda (cukup_baik/kurang)? '),
    read(Tidur),
    (member(Tidur, [cukup_baik, kurang]) -> assertz(waktu_tidur(Tidur)) ; (write('Input tidak valid, gunakan cukup_baik/kurang.'), nl, tanya_waktu_tidur)).

tampilkan_rekomendasi :-
    nl,
    write('Rekomendasi untuk Anda:'), nl,
    write('--- Makanan ---'), nl,
    findall(M-Penjelasan, rekomendasi_makanan(M, Penjelasan), MakananList),
    (MakananList = [] -> write('  Tidak ada rekomendasi makanan yang sesuai.'), nl ;
      forall(member(M-Penjelasan, MakananList), format('  - ~w: ~w~n', [M, Penjelasan]))),

    nl,
    write('--- Olahraga ---'), nl,
    findall(O-Penjelasan, rekomendasi_olahraga(O, Penjelasan), OlahragaList),
    (OlahragaList = [] -> write('  Tidak ada rekomendasi olahraga yang sesuai.'), nl ;
      forall(member(O-Penjelasan, OlahragaList), format('  - ~w: ~w~n', [O, Penjelasan]))),
    nl,
    write('Ingat, ini adalah saran umum. Konsultasikan dengan profesional kesehatan untuk rekomendasi yang dipersonalisasi.'), nl.
