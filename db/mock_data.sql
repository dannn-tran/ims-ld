INSERT INTO items (slug, label, acquire_date, acquire_price, acquire_source)
VALUES 
  (
    'vintage-watch-omega',
    'Vintage Omega Seamaster Watch',
    DATE '2022-06-15',
    ROW('USD', 1250.00)::monetary_amount,
    'eBay seller: classic-timepieces'
  ),
  (
    'antique-lamp-tiffany',
    'Antique Tiffany-Style Lamp',
    DATE '2021-11-02',
    ROW('USD', 890.00)::monetary_amount,
    'Antique Store, Brooklyn'
  ),
  (
    'painting-monet-repro',
    'Monet Reproduction Painting',
    DATE '2023-03-12',
    ROW('EUR', 320.50)::monetary_amount,
    'Paris Street Market'
  );